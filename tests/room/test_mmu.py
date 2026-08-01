from amaranth.sim import Simulator

from room.consts import IssueQueueType
from room.mmu import PageTableWalker

CORE_PARAMS = dict(
    xlen=64,
    flen=64,
    use_fpu=False,
    fma_latency=0,
    vaddr_bits=39,
    paddr_bits=36,
    io_regions={},
    core_width=1,
    use_vm=True,
    use_user=True,
    use_supervisor=True,
    fetch_width=1,
    fetch_buffer_size=4,
    max_br_count=1,
    use_bpd=False,
    bpd_meta_length=1,
    n_ras_entries=1,
    num_int_pregs=32,
    num_fp_pregs=32,
    num_breakpoints=0,
    issue_params={
        IssueQueueType.INT: dict(issue_width=1),
        IssueQueueType.MEM: dict(issue_width=1),
    },
    num_rob_rows=4,
    ldq_size=4,
    stq_size=4,
    pg_levels=3,
    n_dtlb_sets=4,
    n_dtlb_ways=1,
)


def make_pte(*,
             ppn=0,
             valid=True,
             read=False,
             write=False,
             execute=False,
             accessed=False,
             dirty=False,
             reserved=0):
    return ((int(valid) << 0) | (int(read) << 1) | (int(write) << 2)
            | (int(execute) << 3) | (int(accessed) << 6)
            | (int(dirty) << 7) | (ppn << 10) | (reserved << 54))


def run_page_table_walk(vaddr, memory, *, root_ppn=0, cycles=100):
    dut = PageTableWalker(CORE_PARAMS)
    requests = []
    response = {}

    def request_process():
        yield dut.satp.w.mode.eq(8)
        yield dut.satp.w.ppn.eq(root_ppn)
        yield dut.satp.we.eq(1)
        yield
        yield dut.satp.we.eq(0)
        yield

        yield dut.req.bits.vpn.eq(vaddr >> 12)
        yield dut.req.valid.eq(1)
        while not (yield dut.req.ready):
            yield
        yield
        yield dut.req.valid.eq(0)

        for _ in range(cycles):
            if (yield dut.resp.valid):
                response.update(
                    level=(yield dut.resp.bits.level),
                    ppn=(yield dut.resp.bits.pte.ppn),
                    valid=(yield dut.resp.bits.pte.v),
                    read=(yield dut.resp.bits.pte.r),
                    accessed=(yield dut.resp.bits.pte.a),
                    ae_ptw=(yield dut.resp.bits.ae_ptw),
                    ae_leaf=(yield dut.resp.bits.ae_leaf),
                    page_fault=(yield dut.resp.bits.pf),
                )
                return
            yield

        assert False, f"page-table walk timed out after requests {requests}"

    def memory_process():
        pending = None
        yield dut.mem_req.ready.eq(1)
        yield

        for _ in range(cycles):
            yield dut.mem_resp.valid.eq(0)

            if pending is not None:
                delay, data = pending
                if delay == 0:
                    yield dut.mem_resp.bits.data.eq(data)
                    yield dut.mem_resp.valid.eq(1)
                    pending = None
                else:
                    pending = (delay - 1, data)

            if (yield dut.mem_req.valid) and (yield dut.mem_req.ready):
                addr = (yield dut.mem_req.bits.addr)
                requests.append(addr)
                assert pending is None
                pending = (1, memory.get(addr, 0))
            yield

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(request_process)
    sim.add_sync_process(memory_process)
    sim.run()

    assert response
    return requests, response


def test_page_table_walker_translates_three_level_mapping():
    memory = {
        0x0000: make_pte(ppn=1),
        0x1000: make_pte(ppn=2),
        0x2000: make_pte(read=True, accessed=True),
    }

    requests, response = run_page_table_walk(0, memory)

    assert requests == [0x0000, 0x1000, 0x2000]
    assert response == dict(level=2,
                            ppn=0,
                            valid=1,
                            read=1,
                            accessed=1,
                            ae_ptw=0,
                            ae_leaf=0,
                            page_fault=0)


def test_page_table_walker_translates_superpage_mapping():
    memory = {
        0x0010: make_pte(ppn=3),
        0x3000: make_pte(ppn=0x80000, read=True, accessed=True),
    }

    requests, response = run_page_table_walk(0x80000000, memory)

    assert requests == [0x0010, 0x3000]
    assert response["level"] == 1
    assert response["ppn"] == 0x80000
    assert response["ae_ptw"] == 0
    assert response["ae_leaf"] == 0
    assert response["page_fault"] == 0


def test_page_table_walker_reports_invalid_table_address():
    memory = {
        0x0000: make_pte(ppn=1 << 24),
    }

    requests, response = run_page_table_walk(0, memory)

    assert requests == [0x0000]
    assert response["level"] == 0
    assert response["ae_ptw"] == 1
    assert response["ae_leaf"] == 0
    assert response["page_fault"] == 0


def test_page_table_walker_reports_reserved_leaf_pte():
    memory = {
        0x0000: make_pte(read=True, accessed=True, reserved=1),
    }

    requests, response = run_page_table_walk(0, memory)

    assert requests == [0x0000]
    assert response["level"] == 0
    assert response["ae_ptw"] == 0
    assert response["ae_leaf"] == 0
    assert response["page_fault"] == 1
