#ifndef _RTLSIM_TRACER_H_
#define _RTLSIM_TRACER_H_

#include <cstdint>
#include <unordered_map>
#include <set>
#include <mutex>

#include "singleton.h"

namespace room {

class Tracer : public Singleton<Tracer> {
public:
    Tracer();

    void trace_if(int uop_id, uint64_t pc, uint32_t insn);
    void trace_id(int uop_id, int br_mask);
    void trace_ex(int uop_id, int opcode, int prs1, uint64_t rs1_data, int prs2,
                  uint64_t rs2_data);
    void trace_mem(int uop_id, int opcode, uint64_t addr, uint64_t data,
                   int prs1, int prs2);
    void trace_wb(int uop_id, int pdst, uint64_t data);
    void trace_commit(int uop_id);
    void trace_branch_resolve(int resolve_mask);
    void trace_branch_mispredict(int mispredict_mask);
    void trace_flush();

    void tick();

private:
    struct Instruction {
        int uop_id;
        uint64_t pc;
        uint32_t inst;

        int br_mask;

        int pdst;
        int prs1;
        int prs2;
        uint64_t rd_data;
        uint64_t rs1_data;
        uint64_t rs2_data;

        uint64_t addr;
        uint64_t data;
    };

    std::unordered_map<int, Instruction> insts_;
    std::mutex mutex_;

    int resolve_mask_;
    int mispredict_mask_;
    bool flush_;
    std::set<int> commit_ids_;

    void commit(const Instruction& inst);
};

} // namespace room

#endif