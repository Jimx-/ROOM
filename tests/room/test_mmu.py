from amaranth.sim import Simulator
import pytest

from room.consts import IssueQueueType, MemoryCommand, PrivilegeMode
from room.mmu import PageTableWalker
from room.tlb import TLB

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


def make_sv39_mapping(*,
                      root_ppn,
                      address,
                      leaf_level,
                      leaf_ppn,
                      g_stage=False):
    """Build one Sv39 or Sv39x4 path and return its physical reads."""
    vpn = address >> 12
    indices = [
        (vpn >> 18) & ((1 << (11 if g_stage else 9)) - 1),
        (vpn >> 9) & 0x1ff,
        vpn & 0x1ff,
    ]
    next_table_ppn = root_ppn + (4 if g_stage else 1)
    table_ppn = root_ppn
    memory = {}
    requests = []

    for level in range(leaf_level):
        pte_addr = (table_ppn << 12) | (indices[level] << 3)
        requests.append(pte_addr)
        memory[pte_addr] = make_pte(ppn=next_table_ppn)
        table_ppn = next_table_ppn
        next_table_ppn += 1

    pte_addr = (table_ppn << 12) | (indices[leaf_level] << 3)
    requests.append(pte_addr)
    memory[pte_addr] = make_pte(ppn=leaf_ppn,
                                read=True,
                                write=True,
                                execute=True,
                                accessed=True,
                                dirty=True)
    return memory, requests


def run_page_table_walk(vaddr,
                        memory,
                        *,
                        root_ppn=0,
                        vs_root_ppn=0,
                        g_root_ppn=0,
                        vstage1=False,
                        stage2=False,
                        cycles=200):
    hypervisor = vstage1 or stage2
    params = {**CORE_PARAMS, "use_hypervisor": hypervisor}
    dut = PageTableWalker(params)
    requests = []
    response = {}

    def request_process():
        yield dut.satp.w.mode.eq(8)
        yield dut.satp.w.ppn.eq(root_ppn)
        yield dut.satp.we.eq(1)
        yield
        yield dut.satp.we.eq(0)
        yield

        if hypervisor:
            yield dut.vsatp.w.mode.eq(8 if vstage1 else 0)
            yield dut.vsatp.w.ppn.eq(vs_root_ppn)
            yield dut.vsatp.we.eq(1)
            yield
            yield dut.vsatp.we.eq(0)

            yield dut.hgatp.w.mode.eq(8 if stage2 else 0)
            yield dut.hgatp.w.ppn.eq(g_root_ppn)
            yield dut.hgatp.we.eq(1)
            yield
            yield dut.hgatp.we.eq(0)
            yield

        yield dut.req.bits.vpn.eq(vaddr >> 12)
        yield dut.req.bits.vstage1.eq(vstage1)
        yield dut.req.bits.stage2.eq(stage2)
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
                if hypervisor:
                    response.update(
                        guest_fault=(yield dut.resp.bits.gf),
                        gpa=(yield dut.resp.bits.gpa),
                        gpa_is_pte=(yield dut.resp.bits.gpa_is_pte),
                        guest_read=(yield dut.resp.bits.hr),
                        guest_write=(yield dut.resp.bits.hw),
                        guest_execute=(yield dut.resp.bits.hx),
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

    requests, response = run_page_table_walk(0x80123000, memory)

    assert requests == [0x0010, 0x3000]
    assert response["level"] == 1
    assert response["ppn"] == 0x80000
    assert response["ae_ptw"] == 0
    assert response["ae_leaf"] == 0
    assert response["page_fault"] == 0


def test_tlb_merges_low_vpn_bits_into_superpage_ppn():
    params = {**CORE_PARAMS, "n_pmps": 0}
    dut = TLB(req_width=1,
              params=params,
              log_max_size=3,
              n_sets=4,
              n_ways=2,
              n_banks=2,
              n_superpage_entries=2)

    def process():
        req = dut.req[0]
        resp = dut.resp[0]

        yield dut.prv.eq(PrivilegeMode.S)
        yield dut.ptbr.mode.eq(8)
        yield dut.ptw_req.ready.eq(1)
        yield

        vaddr = 0x80123ABC
        yield req.bits.vaddr.eq(vaddr)
        yield req.bits.cmd.eq(MemoryCommand.READ)
        yield req.bits.size.eq(3)
        yield req.valid.eq(1)
        yield
        yield req.valid.eq(0)
        yield

        assert (yield resp.valid)
        assert (yield resp.bits.miss)
        assert (yield dut.ptw_req.valid)
        assert (yield dut.ptw_req.bits.vpn) == vaddr >> 12
        yield

        yield dut.ptw_resp.bits.level.eq(1)
        yield dut.ptw_resp.bits.pte.v.eq(1)
        yield dut.ptw_resp.bits.pte.r.eq(1)
        yield dut.ptw_resp.bits.pte.a.eq(1)
        superpage_ppn = 0x40000
        yield dut.ptw_resp.bits.pte.ppn.eq(superpage_ppn)
        yield dut.ptw_resp.valid.eq(1)
        yield
        yield dut.ptw_resp.valid.eq(0)
        yield

        yield req.bits.vaddr.eq(vaddr)
        yield req.valid.eq(1)
        yield
        yield req.valid.eq(0)
        yield

        assert (yield resp.valid)
        assert not (yield resp.bits.miss)
        superpage_offset = vaddr & ((1 << 21) - 1)
        expected_paddr = (superpage_ppn << 12) | superpage_offset
        assert (yield resp.bits.paddr) == expected_paddr

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(process)
    sim.run()


def test_tlb_aligns_merged_g_stage_ppn_before_superpage_refill():
    params = {
        **CORE_PARAMS,
        "n_pmps": 0,
        "use_hypervisor": True,
    }
    dut = TLB(req_width=1,
              params=params,
              log_max_size=3,
              n_sets=4,
              n_ways=2,
              n_banks=2,
              n_superpage_entries=2)

    def process():
        req = dut.req[0]
        resp = dut.resp[0]

        yield dut.prv.eq(PrivilegeMode.S)
        yield dut.v.eq(1)
        yield dut.vsatp.mode.eq(0)
        yield dut.hgatp.mode.eq(8)
        yield dut.ptw_req.ready.eq(1)
        yield

        first_vaddr = 0x80123ABC
        yield req.bits.vaddr.eq(first_vaddr)
        yield req.bits.cmd.eq(MemoryCommand.READ)
        yield req.bits.size.eq(3)
        yield req.valid.eq(1)
        yield
        yield req.valid.eq(0)
        yield

        assert (yield resp.bits.miss)
        assert (yield dut.ptw_req.valid)
        yield

        # The G-stage PTW returns the exact PPN for the first request, whose
        # low nine bits place it inside this 2-MiB leaf.
        physical_base_ppn = 0x40000
        merged_ppn = physical_base_ppn | ((first_vaddr >> 12) & 0x1ff)
        yield dut.ptw_resp.bits.level.eq(1)
        yield dut.ptw_resp.bits.pte.v.eq(1)
        yield dut.ptw_resp.bits.pte.r.eq(1)
        yield dut.ptw_resp.bits.pte.a.eq(1)
        yield dut.ptw_resp.bits.pte.ppn.eq(merged_ppn)
        yield dut.ptw_resp.bits.hr.eq(1)
        yield dut.ptw_resp.valid.eq(1)
        yield
        yield dut.ptw_resp.valid.eq(0)
        yield

        # A second page in the same superpage must replace, rather than OR
        # with, the first request's low PPN bits.
        second_vaddr = 0x80145ABC
        yield req.bits.vaddr.eq(second_vaddr)
        yield req.valid.eq(1)
        yield
        yield req.valid.eq(0)
        yield

        expected_paddr = ((physical_base_ppn << 12)
                          | (second_vaddr & ((1 << 21) - 1)))
        assert (yield resp.valid)
        assert not (yield resp.bits.miss)
        assert (yield resp.bits.paddr) == expected_paddr

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(process)
    sim.run()


@pytest.mark.parametrize(
    "vs_mode,g_mode,expect_walk,expect_vstage1,expect_stage2", [
        pytest.param(0, 0, False, 0, 0, id="bare-bare"),
        pytest.param(8, 0, True, 1, 0, id="sv39-bare"),
        pytest.param(0, 8, True, 0, 1, id="bare-sv39x4"),
        pytest.param(8, 8, True, 1, 1, id="sv39-sv39x4"),
    ])
def test_tlb_selects_guest_translation_regime(vs_mode, g_mode, expect_walk,
                                              expect_vstage1, expect_stage2):
    params = {**CORE_PARAMS, "n_pmps": 0, "use_hypervisor": True}
    dut = TLB(req_width=1,
              params=params,
              log_max_size=3,
              n_sets=4,
              n_ways=1,
              n_banks=1,
              n_superpage_entries=1)

    def process():
        req = dut.req[0]

        yield dut.prv.eq(PrivilegeMode.S)
        yield dut.v.eq(1)
        yield dut.vsatp.mode.eq(vs_mode)
        yield dut.hgatp.mode.eq(g_mode)
        yield dut.ptw_req.ready.eq(1)
        yield

        yield req.bits.vaddr.eq(0x12345000)
        yield req.bits.cmd.eq(MemoryCommand.READ)
        yield req.bits.size.eq(3)
        yield req.valid.eq(1)
        yield
        yield req.valid.eq(0)
        yield

        assert bool((yield dut.ptw_req.valid)) == expect_walk
        if expect_walk:
            assert (yield dut.ptw_req.bits.vpn) == 0x12345
            assert (yield dut.ptw_req.bits.vstage1) == expect_vstage1
            assert (yield dut.ptw_req.bits.stage2) == expect_stage2

    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(process)
    sim.run()


@pytest.mark.parametrize("leaf_level", [0, 1, 2])
def test_page_table_walker_translates_vs_leaf_at_every_level(leaf_level):
    vaddr = (0x12 << 30) | (0x34 << 21) | (0x56 << 12)
    leaf_ppns = [0x80000, 0x81200, 0x81234]
    memory, expected_requests = make_sv39_mapping(
        root_ppn=0x10,
        address=vaddr,
        leaf_level=leaf_level,
        leaf_ppn=leaf_ppns[leaf_level])

    requests, response = run_page_table_walk(vaddr,
                                             memory,
                                             vs_root_ppn=0x10,
                                             vstage1=True)

    assert requests == expected_requests
    assert response["level"] == leaf_level
    assert response["ppn"] == leaf_ppns[leaf_level]
    assert response["page_fault"] == 0
    assert response["guest_fault"] == 0


@pytest.mark.parametrize("leaf_level", [
    pytest.param(0, id="1-gib"),
    pytest.param(1, id="2-mib"),
    pytest.param(2, id="4-kib"),
])
def test_page_table_walker_translates_g_leaf_at_every_level(leaf_level):
    gpa = (0x601 << 30) | (0x12 << 21) | (0x34 << 12)
    leaf_ppns = [0x80000, 0x81200, 0x81234]
    leaf_ppn = leaf_ppns[leaf_level]
    memory, expected_requests = make_sv39_mapping(root_ppn=0x40,
                                                  address=gpa,
                                                  leaf_level=leaf_level,
                                                  leaf_ppn=leaf_ppn,
                                                  g_stage=True)

    requests, response = run_page_table_walk(gpa,
                                             memory,
                                             g_root_ppn=0x40,
                                             stage2=True)

    low_ppn_bits = (2 - leaf_level) * 9
    low_ppn_mask = (1 << low_ppn_bits) - 1
    expected_ppn = leaf_ppn | ((gpa >> 12) & low_ppn_mask)
    assert requests == expected_requests
    assert response["level"] == leaf_level
    assert response["ppn"] == expected_ppn
    assert response["guest_fault"] == 0
    assert response["page_fault"] == 0


@pytest.mark.parametrize("leaf_level", [0, 1])
def test_page_table_walker_rejects_misaligned_host_superpage(leaf_level):
    vaddr = (0x12 << 30) | (0x34 << 21) | (0x56 << 12)
    memory, _ = make_sv39_mapping(root_ppn=0x10,
                                  address=vaddr,
                                  leaf_level=leaf_level,
                                  leaf_ppn=0x80001)

    _, response = run_page_table_walk(vaddr, memory, root_ppn=0x10)

    assert response["page_fault"] == 1


@pytest.mark.parametrize("leaf_level", [0, 1])
def test_page_table_walker_rejects_misaligned_vs_superpage(leaf_level):
    vaddr = (0x12 << 30) | (0x34 << 21) | (0x56 << 12)
    memory, _ = make_sv39_mapping(root_ppn=0x10,
                                  address=vaddr,
                                  leaf_level=leaf_level,
                                  leaf_ppn=0x80001)

    _, response = run_page_table_walk(vaddr,
                                      memory,
                                      vs_root_ppn=0x10,
                                      vstage1=True)

    assert response["page_fault"] == 1
    assert response["guest_fault"] == 0


@pytest.mark.parametrize("leaf_level", [0, 1])
def test_page_table_walker_rejects_misaligned_g_superpage(leaf_level):
    gpa = (0x601 << 30) | (0x12 << 21) | (0x34 << 12)
    memory, _ = make_sv39_mapping(root_ppn=0x40,
                                  address=gpa,
                                  leaf_level=leaf_level,
                                  leaf_ppn=0x80001,
                                  g_stage=True)

    _, response = run_page_table_walk(gpa,
                                      memory,
                                      g_root_ppn=0x40,
                                      stage2=True)

    assert response["guest_fault"] == 1
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


def test_page_table_walker_stops_at_final_level():
    memory = {
        0x0000: make_pte(ppn=1),
        0x1000: make_pte(ppn=2),
        0x2000: make_pte(ppn=3),
    }

    requests, response = run_page_table_walk(0, memory)

    assert requests == [0x0000, 0x1000, 0x2000]
    assert response["level"] == 2


def test_page_table_walker_translates_vs_stage_and_g_stage():
    # The G-stage root is a valid 16-KiB-aligned Sv39x4 root.  Its first
    # entry is a 1-GiB identity superpage, so every VS page-table access is
    # translated before the PTE is fetched.
    memory = {
        0x4000:
        make_pte(read=True,
                 write=True,
                 execute=True,
                 accessed=True,
                 dirty=True),
        0x1000:
        make_pte(ppn=2),
        0x2000:
        make_pte(ppn=3),
        0x3000:
        make_pte(ppn=0x800, read=True, write=True, accessed=True, dirty=True),
    }

    requests, response = run_page_table_walk(0,
                                             memory,
                                             vs_root_ppn=1,
                                             g_root_ppn=4,
                                             vstage1=True,
                                             stage2=True)

    assert requests == [0x4000, 0x1000, 0x4000, 0x2000, 0x4000, 0x3000, 0x4000]
    assert response["ppn"] == 0x800
    assert response["read"] == 1
    assert response["guest_fault"] == 0
    assert response["page_fault"] == 0
    assert response["guest_read"] == 1
    assert response["guest_write"] == 1


def test_nested_walk_uses_finer_g_stage_leaf_level():
    memory = {
        # Translate the VS root PTE GPA 0x1000 through a 4-KiB G leaf.
        0x4000:
        make_pte(ppn=5),
        0x5000:
        make_pte(ppn=6),
        0x6008:
        make_pte(ppn=0x10,
                 read=True,
                 write=True,
                 execute=True,
                 accessed=True,
                 dirty=True),
        # The VS root is a 1-GiB leaf mapping GVA 0 to GPA 0x40000000.
        0x10000:
        make_pte(ppn=0x40000,
                 read=True,
                 write=True,
                 execute=True,
                 accessed=True,
                 dirty=True),
        # Translate that final GPA through another 4-KiB G leaf.
        0x4008:
        make_pte(ppn=7),
        0x7000:
        make_pte(ppn=8),
        0x8000:
        make_pte(ppn=0x90000,
                 read=True,
                 write=True,
                 execute=True,
                 accessed=True,
                 dirty=True),
    }

    requests, response = run_page_table_walk(0,
                                             memory,
                                             vs_root_ppn=1,
                                             g_root_ppn=4,
                                             vstage1=True,
                                             stage2=True)

    assert requests == [
        0x4000, 0x5000, 0x6008, 0x10000, 0x4008, 0x7000, 0x8000
    ]
    assert response["level"] == 2
    assert response["ppn"] == 0x90000
    assert response["guest_fault"] == 0
    assert response["page_fault"] == 0


def test_page_table_walker_translates_nonzero_vs_vpn_and_x4_root_bits():
    vaddr = (0x12 << 30) | (0x34 << 21) | (0x56 << 12)
    final_gpa_root_index = 0x401
    final_gpa_ppn = final_gpa_root_index << 18
    memory = {
        # Identity-map the low GPA region containing the VS page tables.
        0x4000:
        make_pte(read=True,
                 write=True,
                 execute=True,
                 accessed=True,
                 dirty=True),
        0x1090:
        make_pte(ppn=2),
        0x21a0:
        make_pte(ppn=3),
        0x32b0:
        make_pte(ppn=final_gpa_ppn,
                 read=True,
                 write=True,
                 execute=True,
                 accessed=True,
                 dirty=True),
        # Index 0x401 uses both extra Sv39x4 root-index bits.
        0x6008:
        make_pte(ppn=0x80000,
                 read=True,
                 write=True,
                 execute=True,
                 accessed=True,
                 dirty=True),
    }

    requests, response = run_page_table_walk(vaddr,
                                             memory,
                                             vs_root_ppn=1,
                                             g_root_ppn=4,
                                             vstage1=True,
                                             stage2=True)

    assert requests == [0x4000, 0x1090, 0x4000, 0x21a0, 0x4000, 0x32b0, 0x6008]
    assert response["level"] == 2
    assert response["ppn"] == 0x80000
    assert response["guest_fault"] == 0
    assert response["page_fault"] == 0


def test_page_table_walker_reports_guest_fault_for_vs_pte_fetch():
    requests, response = run_page_table_walk(0, {},
                                             vs_root_ppn=1,
                                             g_root_ppn=4,
                                             vstage1=True,
                                             stage2=True)

    assert requests == [0x4000]
    assert response["guest_fault"] == 1
    assert response["page_fault"] == 0
    assert response["gpa"] == 0x1000
    assert response["gpa_is_pte"] == 1


def test_page_table_walker_reports_guest_fault_for_final_gpa():
    memory = {
        # Root entry zero maps the low GPA region containing VS page tables.
        0x4000:
        make_pte(read=True,
                 write=True,
                 execute=True,
                 accessed=True,
                 dirty=True),
        # Root entry one, used by the final 1-GiB GPA, is left invalid.
        0x1000:
        make_pte(ppn=2),
        0x2000:
        make_pte(ppn=3),
        0x3000:
        make_pte(ppn=0x40000, read=True, accessed=True),
    }

    requests, response = run_page_table_walk(0,
                                             memory,
                                             vs_root_ppn=1,
                                             g_root_ppn=4,
                                             vstage1=True,
                                             stage2=True)

    assert requests[-1] == 0x4008
    assert response["guest_fault"] == 1
    assert response["page_fault"] == 0
    assert response["gpa"] == 0x40000000
    assert response["gpa_is_pte"] == 0
