"""Unit tests for the RDMA responder StateTable.

Covers the state model, range checks, and transaction lock introduced in step 1
of ``rdma_state_plan.md``: the responder-side PSN table that the dropper will
read-modify-write in step 3. These tests drive ``StateTable`` in isolation using
``amaranth.sim`` ``add_sync_process`` coroutines.

Clock model reminder (see AGENTS.md): only a naked ``yield`` advances the clock;
``yield sig`` (read) and ``yield sig.eq(v)`` (write) are in-cycle. Helpers below
apply a request, advance one edge so it is visible, then spin for the response.
"""

from amaranth import Elaboratable, Module, Signal
from amaranth.sim import Simulator

from roomsoc.peripheral.net.infiniband import (ConnectionTable, StateTable,
                                               _ConnectionInitializer)

MAX_QPS = 64


def _lookup(dut, qpn):
    """Issue a lookup and return ``(entry_valid, resp_epsn)``.

    The lookup leaves the RX transaction locked; callers must finish it with a
    matching ``_commit`` before issuing another lookup.
    """
    yield dut.read.bits.qpn.eq(qpn)
    yield dut.read.valid.eq(1)
    for _ in range(8):
        yield
        if (yield dut.read.ready):
            break
    else:
        assert False, f"lookup for QPN {qpn} was not accepted"
    yield dut.read.valid.eq(0)
    for _ in range(8):
        if (yield dut.resp.valid):
            break
        yield
    else:
        assert False, f"lookup for QPN {qpn} produced no response"
    entry_valid = (yield dut.resp.bits.valid)
    resp_epsn = (yield dut.resp.bits.resp_epsn)
    yield  # let the pulse clear
    assert (yield dut.resp.valid) == 0, "lookup response lasted over one cycle"
    return entry_valid, resp_epsn


def _init(dut, qpn, resp_epsn):
    """Issue a connection-initialization write and wait for it to fire."""
    yield dut.init.bits.qpn.eq(qpn)
    yield dut.init.bits.resp_epsn.eq(resp_epsn)
    yield dut.init.valid.eq(1)
    for _ in range(16):
        yield
        if (yield dut.init.ready):
            break
    else:
        assert False, f"init for QPN {qpn} was not accepted"
    yield dut.init.valid.eq(0)
    yield


def _commit(dut, qpn, valid, resp_epsn):
    """Issue an RX commit (write-back) and wait for it to fire."""
    yield dut.commit.bits.qpn.eq(qpn)
    yield dut.commit.bits.valid.eq(valid)
    yield dut.commit.bits.resp_epsn.eq(resp_epsn)
    yield dut.commit.valid.eq(1)
    for _ in range(16):
        yield
        if (yield dut.commit.ready):
            break
    else:
        assert False, f"commit for QPN {qpn} was not accepted"
    yield dut.commit.valid.eq(0)
    yield


def _run(dut, process):
    sim = Simulator(dut)
    sim.add_clock(1e-6)
    sim.add_sync_process(process)
    sim.run()


def _connection_write(dut,
                      local_qpn,
                      remote_qpn,
                      remote_ip,
                      remote_port,
                      initial_rx_psn=0):
    yield dut.write.bits.local_qpn.eq(local_qpn)
    yield dut.write.bits.remote_qpn.eq(remote_qpn)
    yield dut.write.bits.remote_ip.eq(remote_ip)
    yield dut.write.bits.remote_port.eq(remote_port)
    yield dut.write.bits.initial_rx_psn.eq(initial_rx_psn)
    yield dut.write.valid.eq(1)
    yield
    assert (yield dut.write.ready) == 1
    yield dut.write.valid.eq(0)
    yield


def _connection_read(dut, qpn):
    yield dut.read.bits.eq(qpn)
    yield dut.read.valid.eq(1)
    yield
    assert (yield dut.read.ready) == 1
    yield dut.read.valid.eq(0)
    for _ in range(8):
        if (yield dut.resp.valid):
            result = ((yield dut.resp.bits.remote_qpn),
                      (yield dut.resp.bits.remote_ip),
                      (yield dut.resp.bits.remote_port))
            yield
            assert (yield dut.resp.valid) == 0
            return result
        yield
    assert False, f"connection lookup for QPN {qpn} produced no response"


def test_reset_entries_are_invalid():
    dut = StateTable(max_qps=MAX_QPS)

    def process():
        entry_valid, _ = yield from _lookup(dut, 0)
        assert entry_valid == 0
        yield from _commit(dut, 0, 0, 0)
        entry_valid, _ = yield from _lookup(dut, 17)
        assert entry_valid == 0
        yield from _commit(dut, 17, 0, 0)

    _run(dut, process)


def test_init_then_lookup_returns_programmed_psn():
    dut = StateTable(max_qps=MAX_QPS)

    def process():
        yield from _init(dut, 5, 0x1234)
        entry_valid, epsn = yield from _lookup(dut, 5)
        assert entry_valid == 1
        assert epsn == 0x1234
        yield from _commit(dut, 5, entry_valid, epsn)

    _run(dut, process)


def test_commit_advances_visible_to_next_lookup():
    dut = StateTable(max_qps=MAX_QPS)

    def process():
        yield from _init(dut, 3, 10)
        entry_valid, epsn = yield from _lookup(dut, 3)
        assert (entry_valid, epsn) == (1, 10)

        yield from _commit(dut, 3, 1, 11)
        entry_valid, epsn = yield from _lookup(dut, 3)
        assert (entry_valid, epsn) == (1, 11)
        yield from _commit(dut, 3, entry_valid, epsn)

    _run(dut, process)


def test_qps_are_isolated():
    dut = StateTable(max_qps=MAX_QPS)

    def process():
        yield from _init(dut, 2, 5)
        yield from _init(dut, 9, 9)

        entry_valid, epsn = yield from _lookup(dut, 2)
        assert (entry_valid, epsn) == (1, 5)
        yield from _commit(dut, 2, entry_valid, epsn)
        entry_valid, epsn = yield from _lookup(dut, 9)
        assert (entry_valid, epsn) == (1, 9)
        yield from _commit(dut, 9, entry_valid, epsn)

    _run(dut, process)


def test_out_of_range_qpn_is_rejected_not_aliased():
    dut = StateTable(max_qps=MAX_QPS)

    def process():
        # Program a low QPN whose index a too-large QPN would alias.
        yield from _init(dut, 1, 0x55)

        # A lookup at max_qps must read back invalid.
        entry_valid, _ = yield from _lookup(dut, MAX_QPS)
        assert entry_valid == 0
        yield from _commit(dut, MAX_QPS, 0, 0)

        # QPN max_qps+1 aliases entry 1's index; it must still read invalid.
        entry_valid, _ = yield from _lookup(dut, MAX_QPS + 1)
        assert entry_valid == 0
        yield from _commit(dut, MAX_QPS + 1, 0, 0)

        # The programmed entry 1 is unaffected by the aliased lookups.
        entry_valid, epsn = yield from _lookup(dut, 1)
        assert (entry_valid, epsn) == (1, 0x55)
        yield from _commit(dut, 1, entry_valid, epsn)

    _run(dut, process)


def test_out_of_range_init_is_ignored():
    dut = StateTable(max_qps=MAX_QPS)

    def process():
        # Invalid setup requests are consumed and ignored rather than wedging
        # their producer forever.
        yield from _init(dut, MAX_QPS, 0x99)

        # Nothing was written: the entry (and its alias) stays invalid.
        entry_valid, _ = yield from _lookup(dut, 0)
        assert entry_valid == 0
        yield from _commit(dut, 0, 0, 0)

    _run(dut, process)


def test_boundary_psn_values_can_be_stored():
    dut = StateTable(max_qps=MAX_QPS)

    def process():
        yield from _init(dut, 0, 0xffffff)
        entry_valid, epsn = yield from _lookup(dut, 0)
        assert (entry_valid, epsn) == (1, 0xffffff)

        yield from _commit(dut, 0, 1, (0xffffff + 1) & 0xffffff)
        entry_valid, epsn = yield from _lookup(dut, 0)
        assert (entry_valid, epsn) == (1, 0)
        yield from _commit(dut, 0, entry_valid, epsn)

    _run(dut, process)


def test_init_blocked_during_rmw_and_holds_payload():
    dut = StateTable(max_qps=MAX_QPS)

    def process():
        yield from _init(dut, 4, 0x40)

        # Begin an RX read-modify-write: a lookup sets the transaction lock.
        yield dut.read.bits.qpn.eq(4)
        yield dut.read.valid.eq(1)
        yield  # read.fire; busy asserts next cycle
        yield dut.read.valid.eq(0)
        yield  # busy now held; consume/ignore the resp pulse

        # An init attempted mid-RMW must stall (ready low) and hold its payload.
        yield dut.init.bits.qpn.eq(4)
        yield dut.init.bits.resp_epsn.eq(0x11)
        yield dut.init.valid.eq(1)
        yield  # request visible; busy -> ready low
        assert (yield dut.init.ready) == 0
        yield
        assert (yield dut.init.ready) == 0

        # The RX commit releases the lock.
        yield from _commit(dut, 4, 1, 0x40)

        # The held init now proceeds, writing the value it carried all along.
        for _ in range(8):
            if (yield dut.init.ready):
                break
            yield
        assert (yield dut.init.ready) == 1
        yield dut.init.valid.eq(0)
        yield

        entry_valid, epsn = yield from _lookup(dut, 4)
        assert (entry_valid, epsn) == (1, 0x11)
        yield from _commit(dut, 4, entry_valid, epsn)

    _run(dut, process)


def test_non_power_of_two_depth_range_check():
    dut = StateTable(max_qps=5)
    assert dut.index_width == 3

    def process():
        yield from _init(dut, 4, 0x22)
        entry_valid, epsn = yield from _lookup(dut, 4)
        assert (entry_valid, epsn) == (1, 0x22)
        yield from _commit(dut, 4, entry_valid, epsn)

        # QPNs >= max_qps are out of range even though the index bits can
        # represent them; they must read back invalid.
        entry_valid, _ = yield from _lookup(dut, 5)
        assert entry_valid == 0
        yield from _commit(dut, 5, 0, 0)
        entry_valid, _ = yield from _lookup(dut, 7)
        assert entry_valid == 0
        yield from _commit(dut, 7, 0, 0)

    _run(dut, process)


def test_lookup_lock_rejects_other_lookups_and_mismatched_commits():
    dut = StateTable(max_qps=MAX_QPS)

    def process():
        yield from _init(dut, 4, 0x44)
        yield from _init(dut, 5, 0x55)

        entry_valid, epsn = yield from _lookup(dut, 4)
        assert (entry_valid, epsn) == (1, 0x44)

        # A second lookup must remain pending while QP 4 owns the RMW lock.
        yield dut.read.bits.qpn.eq(5)
        yield dut.read.valid.eq(1)
        for _ in range(3):
            yield
            assert (yield dut.read.ready) == 0
            assert (yield dut.resp.valid) == 0

        # A commit for a different QP cannot write memory or release the lock.
        yield dut.commit.bits.qpn.eq(5)
        yield dut.commit.bits.valid.eq(1)
        yield dut.commit.bits.resp_epsn.eq(0xaa)
        yield dut.commit.valid.eq(1)
        for _ in range(3):
            yield
            assert (yield dut.commit.ready) == 0
            assert (yield dut.read.ready) == 0
        yield dut.commit.valid.eq(0)

        # The matching commit releases the lock. The held lookup then fires.
        yield dut.commit.bits.qpn.eq(4)
        yield dut.commit.bits.resp_epsn.eq(0x45)
        yield dut.commit.valid.eq(1)
        yield
        assert (yield dut.commit.ready) == 1
        yield dut.commit.valid.eq(0)
        yield
        assert (yield dut.read.ready) == 1
        yield dut.read.valid.eq(0)

        for _ in range(8):
            if (yield dut.resp.valid):
                break
            yield
        else:
            assert False, "held lookup produced no response"
        assert ((yield dut.resp.bits.valid),
                (yield dut.resp.bits.resp_epsn)) == (1, 0x55)
        yield
        yield from _commit(dut, 5, 1, 0x55)

    _run(dut, process)


def test_unsolicited_commit_is_rejected_without_writing():
    dut = StateTable(max_qps=MAX_QPS)

    def process():
        yield from _init(dut, 2, 0x20)

        yield dut.commit.bits.qpn.eq(2)
        yield dut.commit.bits.valid.eq(1)
        yield dut.commit.bits.resp_epsn.eq(0x99)
        yield dut.commit.valid.eq(1)
        for _ in range(3):
            yield
            assert (yield dut.commit.ready) == 0
        yield dut.commit.valid.eq(0)
        yield

        entry_valid, epsn = yield from _lookup(dut, 2)
        assert (entry_valid, epsn) == (1, 0x20)
        yield from _commit(dut, 2, entry_valid, epsn)

    _run(dut, process)


def test_commit_can_invalidate_entry():
    dut = StateTable(max_qps=MAX_QPS)

    def process():
        yield from _init(dut, 7, 0x70)
        entry_valid, epsn = yield from _lookup(dut, 7)
        assert (entry_valid, epsn) == (1, 0x70)
        yield from _commit(dut, 7, 0, 0)

        entry_valid, _ = yield from _lookup(dut, 7)
        assert entry_valid == 0
        yield from _commit(dut, 7, 0, 0)

    _run(dut, process)


def test_single_entry_table_uses_one_address_bit_and_checks_range():
    dut = StateTable(max_qps=1)
    assert dut.index_width == 1

    def process():
        yield from _init(dut, 0, 0xabc)
        entry_valid, epsn = yield from _lookup(dut, 0)
        assert (entry_valid, epsn) == (1, 0xabc)
        yield from _commit(dut, 0, entry_valid, epsn)

        yield from _init(dut, 1, 0xdef)
        entry_valid, _ = yield from _lookup(dut, 1)
        assert entry_valid == 0
        yield from _commit(dut, 1, 0, 0)

    _run(dut, process)


def test_connection_table_range_checks_prevent_aliasing():
    dut = ConnectionTable(max_qps=MAX_QPS)

    def process():
        yield from _connection_write(dut, 0, 10, 0x1111, 1000)
        yield from _connection_write(dut, 1, 11, 0x2222, 2000)

        # These invalid writes would alias entries 0 and 1 if the memory
        # address were merely truncated.
        yield from _connection_write(dut, MAX_QPS, 20, 0xaaaa, 3000)
        yield from _connection_write(dut, MAX_QPS + 1, 21, 0xbbbb, 4000)

        assert (yield from _connection_read(dut, 0)) == (10, 0x1111, 1000)
        assert (yield from _connection_read(dut, 1)) == (11, 0x2222, 2000)
        assert (yield from _connection_read(dut, MAX_QPS)) == (0, 0, 0)
        assert (yield from _connection_read(dut, MAX_QPS + 1)) == (0, 0, 0)

    _run(dut, process)


def test_connection_initializer_waits_for_both_tables_atomically():
    dut = _ConnectionInitializer(max_qps=MAX_QPS)

    class Harness(Elaboratable):

        def elaborate(self, platform):
            m = Module()
            m.submodules.dut = dut
            heartbeat = Signal()
            m.d.sync += heartbeat.eq(~heartbeat)
            return m

    def process():
        req = dut.req
        yield req.bits.local_qpn.eq(7)
        yield req.bits.remote_qpn.eq(17)
        yield req.bits.remote_ip.eq(0x12345678)
        yield req.bits.remote_port.eq(4791)
        yield req.bits.initial_rx_psn.eq(0xabcdef)
        yield req.valid.eq(1)
        yield dut.conn_write.ready.eq(1)
        yield dut.state_init.ready.eq(0)
        yield

        # Routing must not commit early while responder-state init is blocked.
        for _ in range(3):
            assert (yield req.ready) == 0
            assert (yield dut.conn_write.valid) == 0
            assert (yield dut.state_init.valid) == 1
            yield

        yield dut.state_init.ready.eq(1)
        yield
        assert (yield req.ready) == 1
        assert (yield dut.conn_write.valid) == 1
        assert (yield dut.state_init.valid) == 1
        assert (yield dut.conn_write.bits.local_qpn) == 7
        assert (yield dut.conn_write.bits.remote_qpn) == 17
        assert (yield dut.conn_write.bits.remote_ip) == 0x12345678
        assert (yield dut.conn_write.bits.remote_port) == 4791
        assert (yield dut.state_init.bits.qpn) == 7
        assert (yield dut.state_init.bits.resp_epsn) == 0xabcdef
        yield req.valid.eq(0)
        yield

        # Invalid QPNs are consumed, but neither child sees a transaction.
        yield req.bits.local_qpn.eq(MAX_QPS)
        yield req.valid.eq(1)
        yield
        assert (yield req.ready) == 1
        assert (yield dut.conn_write.valid) == 0
        assert (yield dut.state_init.valid) == 0
        yield req.valid.eq(0)
        yield

    _run(Harness(), process)
