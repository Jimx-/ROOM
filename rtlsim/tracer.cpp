#include "tracer.h"

#include "spdlog/spdlog.h"

namespace room {

template <> Tracer* Singleton<Tracer>::m_singleton = nullptr;

Tracer::Tracer() : resolve_mask_(0), mispredict_mask_(0), flush_(false) {}

void Tracer::trace_if(int uop_id, uint64_t pc, uint32_t insn)
{
    std::lock_guard<std::mutex> lock(mutex_);

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
    std::lock_guard<std::mutex> lock(mutex_);

    auto it = insts_.find(uop_id);
    if (it == insts_.end()) {
        spdlog::error("Micro-op {} not found", uop_id);
    }

    it->second.br_mask = br_mask;
}

void Tracer::trace_ex(int uop_id, int opcode, int prs1, uint64_t rs1_data,
                      int prs2, uint64_t rs2_data)
{
    std::lock_guard<std::mutex> lock(mutex_);

    auto it = insts_.find(uop_id);
    if (it == insts_.end()) {
        spdlog::error("Micro-op {} not found", uop_id);
    }

    it->second.prs1 = prs1;
    it->second.prs2 = prs2;
    it->second.rs1_data = rs1_data;
    it->second.rs2_data = rs2_data;
}

void Tracer::trace_mem(int uop_id, int opcode, uint64_t addr, uint64_t data,
                       int prs1, int prs2)
{
    std::lock_guard<std::mutex> lock(mutex_);

    auto it = insts_.find(uop_id);
    if (it == insts_.end()) {
        spdlog::error("Micro-op {} not found", uop_id);
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
    std::lock_guard<std::mutex> lock(mutex_);

    auto it = insts_.find(uop_id);
    if (it == insts_.end()) {
        spdlog::error("Micro-op {} not found", uop_id);
    }

    it->second.pdst = pdst;
    it->second.rd_data = data;
}

void Tracer::trace_commit(int uop_id)
{
    std::lock_guard<std::mutex> lock(mutex_);
    commit_ids_.insert(uop_id);
}

void Tracer::trace_branch_resolve(int resolve_mask)
{
    std::lock_guard<std::mutex> lock(mutex_);
    resolve_mask_ |= resolve_mask;
}

void Tracer::trace_branch_mispredict(int mispredict_mask)
{
    std::lock_guard<std::mutex> lock(mutex_);
    mispredict_mask_ |= mispredict_mask;
}

void Tracer::trace_flush()
{
    std::lock_guard<std::mutex> lock(mutex_);
    flush_ = true;
}

void Tracer::tick()
{
    std::lock_guard<std::mutex> lock(mutex_);

    if (mispredict_mask_ != 0) {
        auto it = insts_.begin();

        while (it != insts_.end()) {
            if (it->second.br_mask & mispredict_mask_)
                it = insts_.erase(it);
            else
                it++;
        }
    }

    if (resolve_mask_ != 0) {
        for (auto& [uop_id, inst] : insts_) {
            inst.br_mask &= ~resolve_mask_;
        }
    }

    if (flush_) {
        insts_.clear();
    }

    mispredict_mask_ = 0;
    resolve_mask_ = 0;
    flush_ = 0;

    for (auto&& uop_id : commit_ids_) {
        auto it = insts_.find(uop_id);

        if (it != insts_.end()) commit(it->second);
    }

    commit_ids_.clear();
}

void Tracer::commit(const Instruction& inst)
{
    spdlog::trace("Commit instruction pc={:#x} inst={:#x} wdata={:#x}", inst.pc,
                  inst.inst, inst.rd_data);
}

} // namespace room

extern "C" void trace_if(int uop_id, uint64_t pc, uint32_t insn)
{
#ifdef ITRACE
    room::Tracer::get_singleton().trace_if(uop_id, pc, insn);
#endif
}

extern "C" void trace_id(int uop_id, int br_mask)
{
#ifdef ITRACE
    room::Tracer::get_singleton().trace_id(uop_id, br_mask);
#endif
}

extern "C" void trace_ex(int uop_id, int opcode, int prs1, uint64_t rs1_data,
                         int prs2, uint64_t rs2_data)
{
#ifdef ITRACE
    room::Tracer::get_singleton().trace_ex(uop_id, opcode, prs1, rs1_data, prs2,
                                           rs2_data);
#endif
}

extern "C" void trace_mem(int uop_id, int opcode, uint64_t addr, uint64_t data,
                          int prs1, int prs2)
{
#ifdef ITRACE
    room::Tracer::get_singleton().trace_mem(uop_id, opcode, addr, data, prs1,
                                            prs2);
#endif
}

extern "C" void trace_wb(int uop_id, int pdst, uint64_t data)
{
#ifdef ITRACE
    room::Tracer::get_singleton().trace_wb(uop_id, pdst, data);
#endif
}

extern "C" void trace_commit(int uop_id)
{
#ifdef ITRACE
    room::Tracer::get_singleton().trace_commit(uop_id);
#endif
}

extern "C" void trace_branch_resolve(int resolve_mask)
{
#ifdef ITRACE
    room::Tracer::get_singleton().trace_branch_resolve(resolve_mask);
#endif
}

extern "C" void trace_branch_mispredict(int mispredict_mask)
{
#ifdef ITRACE
    room::Tracer::get_singleton().trace_branch_mispredict(mispredict_mask);
#endif
}

extern "C" void trace_flush(void)
{
#ifdef ITRACE
    room::Tracer::get_singleton().trace_flush();
#endif
}