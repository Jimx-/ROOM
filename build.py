from room import Wrapper

if __name__ == "__main__":
    from amaranth.back import verilog
    wrapper = Wrapper()

    with open('rtl/wrapper.v', 'w') as f:
        f.write(
            verilog.convert(wrapper,
                            ports=[
                                wrapper.instr_axi.ar.valid,
                                wrapper.instr_axi.ar.addr,
                                wrapper.instr_axi.ar.len,
                                wrapper.instr_axi.ar.size,
                                wrapper.instr_axi.ar.burst,
                                wrapper.instr_axi.ar.lock,
                                wrapper.instr_axi.ar.cache,
                                wrapper.instr_axi.ar.prot,
                                wrapper.instr_axi.ar.qos,
                                wrapper.instr_axi.ar.id,
                                wrapper.instr_axi.ar.ready,
                                wrapper.instr_axi.r.valid,
                                wrapper.instr_axi.r.data,
                                wrapper.instr_axi.r.resp,
                                wrapper.instr_axi.r.last,
                                wrapper.instr_axi.r.ready,
                                wrapper.instr_axi.aw.valid,
                                wrapper.instr_axi.aw.addr,
                                wrapper.instr_axi.aw.len,
                                wrapper.instr_axi.aw.size,
                                wrapper.instr_axi.aw.burst,
                                wrapper.instr_axi.aw.lock,
                                wrapper.instr_axi.aw.cache,
                                wrapper.instr_axi.aw.prot,
                                wrapper.instr_axi.aw.qos,
                                wrapper.instr_axi.aw.id,
                                wrapper.instr_axi.aw.ready,
                                wrapper.instr_axi.w.valid,
                                wrapper.instr_axi.w.data,
                                wrapper.instr_axi.w.strb,
                                wrapper.instr_axi.w.last,
                                wrapper.instr_axi.w.ready,
                                wrapper.instr_axi.b.valid,
                                wrapper.instr_axi.b.resp,
                                wrapper.instr_axi.b.ready,
                            ],
                            name='wrapper'))
