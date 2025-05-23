#ifndef _RTLSIM_TRACER_H_
#define _RTLSIM_TRACER_H_

#include <cstdint>
#include <unordered_map>
#include <string>
#include <memory>
#include <fstream>

#include "singleton.h"

#ifdef DROMAJO
#include "dromajo_cosim.h"
#endif

namespace room {

class Tracer : public Singleton<Tracer> {
public:
    Tracer(uint64_t memory_addr, size_t memory_size, std::string_view bootrom,
           std::string_view dtb, std::string_view initrd,
           std::string_view trace_log_path);
    ~Tracer();

    void trace_if(int uop_id, uint64_t pc, uint32_t insn);
    void trace_id(int uop_id, int br_mask);
    void trace_ex(int uop_id, int opcode, int prs1, uint64_t rs1_data, int prs2,
                  uint64_t rs2_data);
    void trace_mem(int uop_id, int opcode, uint64_t addr, uint64_t data,
                   int prs1, int prs2);
    void trace_wb(int uop_id, int pdst, uint64_t data);
    void trace_commit(int uop_id);
    void trace_exception(int64_t cause, uint32_t insn);
    void trace_branch_mispredict(int mispredict_mask);
    void trace_branch_resolve(int resolve_mask);
    void trace_flush();

    void tick();

    bool should_stop() const { return should_stop_; }

private:
    struct Instruction {
        int uop_id;
        uint64_t pc;
        uint32_t inst;

        bool decoded;
        int br_mask;

        int pdst;
        int prs1;
        int prs2;
        uint64_t rd_data;
        uint64_t rs1_data;
        uint64_t rs2_data;

        uint64_t addr;
        uint64_t data;

        Instruction() : decoded(false), br_mask(0), rd_data(0), addr(0), data(0)
        {}
    };

    std::unordered_map<int, Instruction> insts_;
    bool should_stop_;

    uint64_t cycle_, last_commit_cycle_;

    std::unique_ptr<std::ofstream> trace_log_;

#ifdef DROMAJO
    dromajo_cosim_state_t* dromajo_state_;
#endif

    void commit(const Instruction& inst);
};

} // namespace room

#endif
