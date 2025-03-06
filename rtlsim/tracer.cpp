#include "tracer.h"

#include <cstdarg>
#include <cstdlib>
#include <unistd.h>

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

Tracer::Tracer(uint64_t memory_addr, size_t memory_size,
               std::string_view bootrom, std::string_view dtb,
               std::string_view trace_log_path)
    : should_stop_(false), cycle_(0), last_commit_cycle_(0)
{
    if (!trace_log_path.empty()) {
        trace_log_ =
            std::make_unique<std::ofstream>(std::string(trace_log_path));
    }

#ifdef DROMAJO
    char tempname[] = "/tmp/rtlsim-XXXXXX";

    int tmp_fd = ::mkstemp(tempname);
    if (tmp_fd < 0) {
        spdlog::error("Failed to create temporary Dromajo config file");
        throw std::runtime_error(
            "Failed to create temporary Dromajo config file");
    }

    FILE* cfg_file = fdopen(tmp_fd, "w");

    fputs("{\n", cfg_file);
    fputs("\"version\":1,\n", cfg_file);
    fputs("\"machine\":\"riscv64\",\n", cfg_file);
    fprintf(cfg_file, "\"memory_size\": %lu,\n", (uint64_t)memory_size >> 20UL);
    fprintf(cfg_file, "\"memory_base_addr\": 0x%lx,\n", memory_addr);
    fprintf(cfg_file, "\"bios\": \"%s\"\n", bootrom.data());
    fputs("}\n", cfg_file);

    fclose(cfg_file);

    std::vector<char*> argv{(char*)"rtlsim"};
    std::string dtb_str(dtb);

    if (!dtb.empty()) {
        argv.push_back((char*)"--dtb");
        argv.push_back(dtb_str.data());
    }

    argv.push_back(tempname);

    dromajo_state_ = dromajo_cosim_init(argv.size(), &argv[0]);

    ::unlink(tempname);

    dromajo_install_new_loggers(dromajo_state_, dromajo_debug_log,
                                dromajo_error_log);
#endif
}

Tracer::~Tracer()
{
#ifdef DROMAJO
    dromajo_cosim_fini(dromajo_state_);
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

    if (trace_log_) {
        (*trace_log_) << fmt::format("I {} {:x} {:x}", uop_id, pc, insn)
                      << std::endl;
    }
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

    if (trace_log_) {
        (*trace_log_) << fmt::format("ID {} {:x}", uop_id, br_mask)
                      << std::endl;
    }
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

    if (trace_log_) {
        (*trace_log_) << fmt::format("EX {} {} {} {:x} {} {:x}", uop_id, opcode,
                                     prs1, rs1_data, prs2, rs2_data)
                      << std::endl;
    }
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

    if (trace_log_) {
        (*trace_log_) << fmt::format("MEM {} {} {} {} {:x} {:x}", uop_id,
                                     opcode, prs1, prs2, addr, data)
                      << std::endl;
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

    if (trace_log_) {
        (*trace_log_) << fmt::format("WB {} {} {:x}", uop_id, pdst, data)
                      << std::endl;
    }
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

    if (trace_log_) {
        (*trace_log_) << fmt::format("C {}", uop_id) << std::endl;
    }
}

void Tracer::trace_exception(int64_t cause)
{
#ifdef DROMAJO
    dromajo_cosim_raise_trap(dromajo_state_, 0, cause);
#endif
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

    if (trace_log_) {
        (*trace_log_) << fmt::format("BRK {:x}", mispredict_mask) << std::endl;
    }
}

void Tracer::trace_branch_resolve(int resolve_mask)
{
    for (auto& [uop_id, inst] : insts_) {
        inst.br_mask &= ~resolve_mask;
    }

    if (trace_log_) {
        (*trace_log_) << fmt::format("BRR {:x}", resolve_mask) << std::endl;
    }
}

void Tracer::trace_flush()
{
    insts_.clear();

    if (trace_log_) {
        (*trace_log_) << "X" << std::endl;
    }
}

void Tracer::tick()
{
    cycle_++;

    if (cycle_ - last_commit_cycle_ > 10000) {
        spdlog::error("No committed instruction in the last 10000 cycles, last "
                      "cycle = {}",
                      last_commit_cycle_);
        should_stop_ = true;
    }

    if (trace_log_) {
        (*trace_log_) << "+" << std::endl;
    }
}

void Tracer::commit(const Instruction& inst)
{
    spdlog::trace("Commit instruction pc={:#x} inst={:#x} wdata={:#x}", inst.pc,
                  inst.inst, inst.rd_data);

    last_commit_cycle_ = cycle_;

#ifdef DROMAJO
    int exit_code = dromajo_cosim_step(dromajo_state_, 0, inst.pc, inst.inst,
                                       inst.rd_data, 0, true);
    if (exit_code != 0) {
        spdlog::error("Mismatch with ref model");
        should_stop_ = true;
    }
#endif
}

} // namespace room

extern "C" void dpi_trace_if(int uop_id, uint64_t pc, uint32_t insn)
{
    spdlog::trace("IF uop_id={} pc={:#x} insn={:#x}", uop_id, pc, insn);
#ifdef ITRACE
    room::Tracer::get_singleton().trace_if(uop_id, pc, insn);
#endif
}

extern "C" void dpi_trace_id(int uop_id, int br_mask)
{
    spdlog::trace("ID uop_id={} br_mask={:#x}", uop_id, br_mask);
#ifdef ITRACE
    room::Tracer::get_singleton().trace_id(uop_id, br_mask);
#endif
}

extern "C" void dpi_trace_ex(int uop_id, int opcode, int prs1,
                             uint64_t rs1_data, int prs2, uint64_t rs2_data)
{
    spdlog::trace("EX uop_id={}", uop_id);
#ifdef ITRACE
    room::Tracer::get_singleton().trace_ex(uop_id, opcode, prs1, rs1_data, prs2,
                                           rs2_data);
#endif
}

extern "C" void dpi_trace_mem(int uop_id, int opcode, uint64_t addr,
                              uint64_t data, int prs1, int prs2)
{
    spdlog::trace("MEM uop_id={}", uop_id);
#ifdef ITRACE
    room::Tracer::get_singleton().trace_mem(uop_id, opcode, addr, data, prs1,
                                            prs2);
#endif
}

extern "C" void dpi_trace_wb(int uop_id, int pdst, uint64_t data)
{
    spdlog::trace("WB uop_id={} pdst={} data={:#x}", uop_id, pdst, data);
#ifdef ITRACE
    room::Tracer::get_singleton().trace_wb(uop_id, pdst, data);
#endif
}

extern "C" void dpi_trace_commit(int uop_id)
{
    spdlog::trace("C uop_id={}", uop_id);
#ifdef ITRACE
    room::Tracer::get_singleton().trace_commit(uop_id);
#endif
}

extern "C" void dpi_trace_exception(int64_t cause, uint32_t insn)
{
    spdlog::trace("EXC cause={} insn={:#x}", cause, insn);
#ifdef ITRACE
    room::Tracer::get_singleton().trace_exception(cause);
#endif
}

extern "C" void dpi_trace_branch_mispredict(int mispredict_mask)
{
    spdlog::trace("BRK mask={:#x}", mispredict_mask);
#ifdef ITRACE
    room::Tracer::get_singleton().trace_branch_mispredict(mispredict_mask);
#endif
}

extern "C" void dpi_trace_branch_resolve(int resolve_mask)
{
    spdlog::trace("BRR mask={:#x}", resolve_mask);
#ifdef ITRACE
    room::Tracer::get_singleton().trace_branch_resolve(resolve_mask);
#endif
}

extern "C" void dpi_trace_flush(void)
{
    spdlog::trace("FLUSH");
#ifdef ITRACE
    room::Tracer::get_singleton().trace_flush();
#endif
}
