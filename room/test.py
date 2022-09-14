from amaranth.sim import Simulator


def run_test(dut, proc, sync=False):
    sim = Simulator(dut)
    if sync:
        sim.add_clock(1e-6)
        sim.add_sync_process(proc)
    else:
        sim.add_process(proc)
    sim.run()
