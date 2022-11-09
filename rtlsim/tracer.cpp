#include "tracer.h"

#include <cstdarg>

#include "spdlog/spdlog.h"

namespace room {

template <> Tracer* Singleton<Tracer>::m_singleton = nullptr;

#ifdef DROMAJO
static void dromajo_debug_log(int hartid, const char* fmt, ...)
{
    va_list args;
    char buf[256];
    int n;

    va_start(args, fmt);
    n = vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);

    if (n > 0) {
        while (--n) {
            if (buf[n] == '\n' || buf[n] == '\r')
                buf[n] = '\0';
            else
                break;
        }
    }

    spdlog::debug("[hartid={}] {}", hartid, buf);
}

static void dromajo_error_log(int hartid, const char* fmt, ...)
{
    va_list args;
    char buf[256];
    int n;

    va_start(args, fmt);
    n = vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);

    if (n > 0) {
        while (--n) {
            if (buf[n] == '\n' || buf[n] == '\r')
                buf[n] = '\0';
            else
                break;
        }
    }

    spdlog::error("[hartid={}] {}", hartid, buf);
}
#endif

Tracer::Tracer(std::string_view dromajo_cfg)
{
#ifdef DROMAJO
    char* argv[] = {(char*)"rtlsim", (char*)dromajo_cfg.data()};
    dromajo_state = dromajo_cosim_init(2, argv);

    dromajo_install_new_loggers(dromajo_state, dromajo_debug_log,
                                dromajo_error_log);
#endif
}

Tracer::~Tracer()
{
#ifdef DROMAJO
    dromajo_cosim_fini(dromajo_state);
#endif
}

void Tracer::trace_if(int uop_id, uint64_t pc, uint32_t insn)
{
    if (insts_.find(uop_id) != insts_.end()) {
        spdlog::error("Duplicate micro-op ID: {}", uop_id);
        return;
    }

    Instruction inst;
    inst.uop_id = uop_id;
    inst.pc = pc;
    inst.inst = insn;

    insts_.emplace(uop_id, inst);
}

void Tracer::trace_id(int uop_id, int br_mask)
{
    auto it = insts_.find(uop_id);
    if (it == insts_.end()) {
        spdlog::error("Micro-op {} not found", uop_id);
        return;
    }

    it->second.br_mask = br_mask;
    it->second.decoded = true;
}

void Tracer::trace_ex(int uop_id, int opcode, int prs1, uint64_t rs1_data,
                      int prs2, uint64_t rs2_data)
{
    auto it = insts_.find(uop_id);
    if (it == insts_.end()) {
        spdlog::error("Micro-op {} not found", uop_id);
        return;
    }

    it->second.prs1 = prs1;
    it->second.prs2 = prs2;
    it->second.rs1_data = rs1_data;
    it->second.rs2_data = rs2_data;
}

void Tracer::trace_mem(int uop_id, int opcode, uint64_t addr, uint64_t data,
                       int prs1, int prs2)
{
    auto it = insts_.find(uop_id);
    if (it == insts_.end()) {
        spdlog::error("Micro-op {} not found", uop_id);
        return;
    }

    it->second.prs1 = prs1;
    it->second.prs2 = prs2;

    if (opcode == 2)
        it->second.addr = addr;
    else if (opcode == 3)
        it->second.data = data;
    else {
        it->second.addr = addr;
        it->second.data = data;
    }
}

void Tracer::trace_wb(int uop_id, int pdst, uint64_t data)
{
    auto it = insts_.find(uop_id);
    if (it == insts_.end()) {
        spdlog::error("Micro-op {} not found", uop_id);
        return;
    }

    it->second.pdst = pdst;
    it->second.rd_data = data;
}

void Tracer::trace_commit(int uop_id)
{
    auto it = insts_.find(uop_id);
    if (it == insts_.end()) {
        spdlog::error("Micro-op {} not found", uop_id);
        return;
    }

    commit(it->second);

    insts_.erase(it);
}

void Tracer::trace_branch_mispredict(int mispredict_mask)
{
    auto it = insts_.begin();

    while (it != insts_.end()) {
        if ((it->second.br_mask & mispredict_mask) || !it->second.decoded)
            it = insts_.erase(it);
        else
            it++;
    }
}

void Tracer::trace_branch_resolve(int resolve_mask)
{

    for (auto& [uop_id, inst] : insts_) {
        inst.br_mask &= ~resolve_mask;
    }
}

void Tracer::trace_flush() { insts_.clear(); }

void Tracer::tick() {}

void Tracer::commit(const Instruction& inst)
{
    spdlog::trace("Commit instruction pc={:#x} inst={:#x} wdata={:#x}", inst.pc,
                  inst.inst, inst.rd_data);

#ifdef DROMAJO
    int exit_code = dromajo_cosim_step(dromajo_state, 0, inst.pc, inst.inst,
                                       inst.rd_data, 0, true);
    if (exit_code != 0) {
        spdlog::error("Mismatch with ref model");
    }
#endif
}

} // namespace room

extern "C" void trace_if(int uop_id, uint64_t pc, uint32_t insn)
{
    spdlog::trace("IF uop_id={} pc={:#x} insn={:#x}", uop_id, pc, insn);
#ifdef ITRACE
    room::Tracer::get_singleton().trace_if(uop_id, pc, insn);
#endif
}

extern "C" void trace_id(int uop_id, int br_mask)
{
    spdlog::trace("ID uop_id={} br_mask={:#x}", uop_id, br_mask);
#ifdef ITRACE
    room::Tracer::get_singleton().trace_id(uop_id, br_mask);
#endif
}

extern "C" void trace_ex(int uop_id, int opcode, int prs1, uint64_t rs1_data,
                         int prs2, uint64_t rs2_data)
{
    spdlog::trace("EX uop_id={}", uop_id);
#ifdef ITRACE
    room::Tracer::get_singleton().trace_ex(uop_id, opcode, prs1, rs1_data, prs2,
                                           rs2_data);
#endif
}

extern "C" void trace_mem(int uop_id, int opcode, uint64_t addr, uint64_t data,
                          int prs1, int prs2)
{
    spdlog::trace("MEM uop_id={}", uop_id);
#ifdef ITRACE
    room::Tracer::get_singleton().trace_mem(uop_id, opcode, addr, data, prs1,
                                            prs2);
#endif
}

extern "C" void trace_wb(int uop_id, int pdst, uint64_t data)
{
    spdlog::trace("WB uop_id={} pdst={} data={:#x}", uop_id, pdst, data);
#ifdef ITRACE
    room::Tracer::get_singleton().trace_wb(uop_id, pdst, data);
#endif
}

extern "C" void trace_commit(int uop_id)
{
    spdlog::trace("C uop_id={}", uop_id);
#ifdef ITRACE
    room::Tracer::get_singleton().trace_commit(uop_id);
#endif
}

extern "C" void trace_branch_mispredict(int mispredict_mask)
{
    spdlog::trace("BRK mask={:#x}", mispredict_mask);
#ifdef ITRACE
    room::Tracer::get_singleton().trace_branch_mispredict(mispredict_mask);
#endif
}

extern "C" void trace_branch_resolve(int resolve_mask)
{
    spdlog::trace("BRR mask={:#x}", resolve_mask);
#ifdef ITRACE
    room::Tracer::get_singleton().trace_branch_resolve(resolve_mask);
#endif
}

extern "C" void trace_flush(void)
{
    spdlog::trace("FLUSH");
#ifdef ITRACE
    room::Tracer::get_singleton().trace_flush();
#endif
}
