#include "soc.h"

#include <memory>
#include <iostream>
#include <list>
#include <queue>

#include <verilated.h>

#include "Vsoc_wrapper.h"
#include "Vsoc_wrapper__Syms.h"

#include "uart.h"
#include "sdcard.h"

#include "tracer.h"

#include "spdlog/spdlog.h"

#ifdef VCD_OUTPUT
#include <verilated_vcd_c.h>
#elif defined(FST_OUTPUT)
#include <verilated_fst_c.h>
#endif

#ifdef RAMULATOR
#include "ramulator/src/Gem5Wrapper.h"
#include "ramulator/src/Request.h"
#include "ramulator/src/Statistics.h"

#ifndef MEM_CYCLE_RATIO
#define MEM_CYCLE_RATIO 0
#endif

#define MEM_BLOCK_BEATS 8
#endif

#ifndef TRACE_START_TIME
#define TRACE_START_TIME 0ull
#endif

#ifndef TRACE_STOP_TIME
#define TRACE_STOP_TIME -1ull
#endif

#ifndef VERILATOR_RESET_VALUE
#define VERILATOR_RESET_VALUE 2
#endif

#define RESET_DELAY 2

namespace room {

static uint64_t timestamp = 0;

double sc_time_stamp() { return timestamp; }

static bool trace_enabled = false;
static uint64_t trace_start_time = TRACE_START_TIME;
static uint64_t trace_stop_time = TRACE_STOP_TIME;

bool sim_trace_enabled()
{
    if (timestamp >= trace_start_time && timestamp < trace_stop_time)
        return true;
    return trace_enabled;
}

void sim_trace_enable(bool enable) { trace_enabled = enable; }

class SoC::Impl {
public:
    Impl(std::string_view sd_image, uint64_t memory_addr, size_t memory_size,
         std::string_view bootrom, std::string_view dtb,
         std::string_view trace_log)
        : ram_addr_(memory_addr)
    {
        Verilated::randReset(VERILATOR_RESET_VALUE);
        Verilated::randSeed(50);

        // turn off assertion before reset
        Verilated::assertOn(false);

        ram_ = &register_ram(memory_addr, (uint64_t)memory_size);

        if (!bootrom.empty()) load_bootrom(bootrom);

        dut_.reset(new Vsoc_wrapper());

        if (!sd_image.empty()) sdcard_.reset(new SDCard(sd_image));

#ifdef ITRACE
        (void)new Tracer(memory_addr, memory_size, bootrom, dtb, trace_log);
#endif

#ifdef VCD_OUTPUT
        Verilated::traceEverOn(true);
        trace_ = new VerilatedVcdC();
        dut_->trace(trace_, 99);
        trace_->open("trace.vcd");
#elif defined(FST_OUTPUT)
        Verilated::traceEverOn(true);
        trace_ = new VerilatedFstC();
        dut_->trace(trace_, 99);
        trace_->open("trace.fst");
#endif

#ifdef RAMULATOR
        ramulator::Config ram_config;
        ram_config.add("standard", "DDR4");
        ram_config.add("channels", "8");
        ram_config.add("ranks", "1");
        ram_config.add("speed", "DDR4_2400R");
        ram_config.add("org", "DDR4_4Gb_x8");
        ram_config.add("mapping", "defaultmapping");
        ram_config.set_core_num(1);
        dram_ = new ramulator::Gem5Wrapper(ram_config, 64);
        Stats::statlist.output("ramulator.log");
#endif
    }

    ~Impl()
    {
#if defined(VCD_OUTPUT) || defined(FST_OUTPUT)
        trace_->close();
        delete trace_;
#endif

#ifdef RAMULATOR
        if (dram_) {
            dram_->finish();
            Stats::statlist.printall();
            delete dram_;
        }
        dram_ = nullptr;
#endif
    }

    int run()
    {
        int N = 2000000;

        this->reset();

        for (int i = 0; i < N; i++) {
            if (i && (i % 10000 == 0)) {
                spdlog::trace("Cycle {}/{}", i, N);
#if defined(VCD_OUTPUT) || defined(FST_OUTPUT)
                trace_->flush();
#endif
            }

            this->tick();

#ifdef ITRACE
            if (Tracer::get_singleton().should_stop()) {
                spdlog::info("Tracer stopped at cycle {}", i);
                break;
            }
#endif
        }

        return 0;
    }

    void memory_read(uint64_t addr, uint64_t* data, int log_size)
    {
        const auto* pr = get_memory_range(addr);

        if (!pr)
            return;
        else if (pr->is_ram) {
            auto* ptr = pr->phys_mem.get() + (ptrdiff_t)(addr - pr->addr);

            switch (log_size) {
            case 0:
                *data = *(uint8_t*)ptr;
                break;
            case 1:
                *data = *(uint16_t*)ptr;
                break;
            case 2:
                *data = *(uint32_t*)ptr;
                break;
            case 3:
                *data = *(uint64_t*)ptr;
                break;
            default:
                spdlog::error("Invalid memory read size {}", 1 << log_size);
                abort();
            }
        }
    }

    void memory_write(uint64_t addr, int byte_mask, uint64_t data, int log_size)
    {
        const auto* pr = get_memory_range(addr);

        uint64_t bit_mask = 0;

        for (int i = 0; i < 1 << log_size; i++) {
            if (byte_mask & (1 << i)) {
                bit_mask |= 0xffUL << (i << 3);
            }
        }

        if (!pr)
            return;
        else if (pr->is_ram) {
            auto* ptr = pr->phys_mem.get() + (ptrdiff_t)(addr - pr->addr);

            switch (log_size) {
            case 0:
                *(uint8_t*)ptr = (uint8_t)((*(uint8_t*)ptr & ~bit_mask) |
                                           ((uint8_t)data & bit_mask));
                break;
            case 1:
                *(uint16_t*)ptr = (uint16_t)((*(uint16_t*)ptr & ~bit_mask) |
                                             ((uint16_t)data & bit_mask));
                break;
            case 2:
                *(uint32_t*)ptr = (uint32_t)((*(uint32_t*)ptr & ~bit_mask) |
                                             ((uint32_t)data & bit_mask));
                break;
            case 3:
                *(uint64_t*)ptr = (uint64_t)((*(uint64_t*)ptr & ~bit_mask) |
                                             ((uint64_t)data & bit_mask));
                break;
            default:
                spdlog::error("Invalid memory write size {}", 1 << log_size);
                abort();
            }
        }
    }

private:
    std::unique_ptr<Vsoc_wrapper> dut_;
    Uart uart_;
    std::unique_ptr<SDCard> sdcard_;
#ifdef VCD_OUTPUT
    VerilatedVcdC* trace_;
#elif defined(FST_OUTPUT)
    VerilatedFstC* trace_;
#endif

    uint64_t ram_addr_;

#ifdef RAMULATOR
    struct MemoryRequest {
        bool ready;
        uint64_t data[MEM_BLOCK_BEATS];
        uint8_t mask[MEM_BLOCK_BEATS];
        uint64_t addr;
        uint64_t tag;
        bool write;
        int beat;

        MemoryRequest(uint64_t addr, uint64_t tag, bool write)
            : addr(addr), tag(tag), write(write), ready(false), beat(0)
        {}
    };

    ramulator::Gem5Wrapper* dram_;
    std::queue<ramulator::Request> dram_queue_;
    std::list<std::unique_ptr<MemoryRequest>> mem_req_queue_;
    std::list<std::unique_ptr<MemoryRequest>> dram_write_queue_;
    bool dram_rd_rsp_active_;
    bool dram_rd_rsp_ready_;
    bool dram_wr_rsp_active_;
    bool dram_wr_rsp_ready_;
#endif

    struct PhysMemoryRange {
        uint64_t addr;
        uint64_t size;
        bool is_ram;
        std::unique_ptr<uint8_t[]> phys_mem;
    };

    std::vector<PhysMemoryRange> mem_map_;
    PhysMemoryRange* ram_;

    PhysMemoryRange& register_ram(uint64_t addr, uint64_t size)
    {
        PhysMemoryRange& pr = mem_map_.emplace_back();

        pr.addr = addr;
        pr.size = size;
        pr.is_ram = true;
        pr.phys_mem = std::make_unique<uint8_t[]>(size);

        return pr;
    }

    const PhysMemoryRange* get_memory_range(uint64_t addr) const
    {
        for (const auto& p : mem_map_) {
            if (addr >= p.addr && addr < p.addr + p.size) return &p;
        }

        return nullptr;
    }

    uint8_t* get_ram_ptr(uint64_t addr)
    {
        auto* pr = get_memory_range(addr);
        if (!pr || !pr->is_ram) return nullptr;
        return pr->phys_mem.get() + (ptrdiff_t)(addr - pr->addr);
    }

    size_t load_bootrom(std::string_view bootrom_name)
    {
        auto* ram_ptr = get_ram_ptr(ram_addr_);
        FILE* f = ::fopen(bootrom_name.data(), "rb");

        if (!f) {
            spdlog::error("Failed to open boot ROM \"{}\"", bootrom_name);
            throw std::runtime_error("Failed to open boot ROM");
        }

        size_t len = ::fread((char*)ram_ptr, 1, ~0U, f);

        ::fclose(f);
        return len;
    }

#ifdef RAMULATOR
    void reset_ram_bus()
    {
        dut_->ram_bus___05Faw___05Fready = 0;
        dut_->ram_bus___05Fw___05Fready = 0;
        dut_->ram_bus___05Far___05Fready = 0;
        dut_->ram_bus___05Fr___05Fvalid = 0;
        dut_->ram_bus___05Fb___05Fvalid = 0;
    }
#endif

    void reset()
    {
        dut_->rst = 1;

#ifdef RAMULATOR
        dram_rd_rsp_active_ = false;
        dram_wr_rsp_active_ = false;

        while (!dram_queue_.empty())
            dram_queue_.pop();
        mem_req_queue_.clear();
        reset_ram_bus();
#endif

        for (int i = 0; i < RESET_DELAY; i++) {
            dut_->clk = 0;
            this->eval(false);
            dut_->clk = 1;
            this->eval(true);
        }

        dut_->rst = 0;
    }

    void tick()
    {
        uart_.tick(dut_->tx);

        if (sdcard_) {
            std::tie(dut_->sdio_cmd_i, dut_->sdio_data_i) =
                sdcard_->tick(dut_->sdio_clk, dut_->sdio_cmd_o,
                              dut_->sdio_cmd_t, dut_->sdio_data_o);
        }

#ifdef ITRACE
        Tracer::get_singleton().tick();
#endif

        dut_->clk = 0;
        this->eval(false);

        dut_->clk = 1;
        this->eval(true);
    }

    void eval(bool clk)
    {
        dut_->eval();

#ifdef RAMULATOR
        eval_ram_bus(clk);
#endif

#if defined(VCD_OUTPUT) || defined(FST_OUTPUT)
        if (sim_trace_enabled()) {
            trace_->dump(timestamp);
        }
#endif

#ifdef RAMULATOR
        if (clk) {
#if MEM_CYCLE_RATIO > 0
            auto cycle = timestamp / 2;
            if (cycle % MEM_CYCLE_RATIO == 0) dram_->tick();
#else
            for (int i = MEM_CYCLE_RATIO; i <= 0; i++)
                dram_->tick();
#endif

            if (!dram_queue_.empty()) {
                if (dram_->send(dram_queue_.front())) {
                    dram_queue_.pop();
                }
            }
        }
#endif

        ++timestamp;
    }

#ifdef RAMULATOR
    void eval_ram_bus(bool clk)
    {
        if (!clk) {
            dram_rd_rsp_ready_ = dut_->ram_bus___05Fr___05Fready;
            dram_wr_rsp_ready_ = dut_->ram_bus___05Fb___05Fready;
            return;
        }

        if (dram_rd_rsp_active_ && dut_->ram_bus___05Fr___05Fvalid &&
            dram_rd_rsp_ready_) {
            dram_rd_rsp_active_ = false;
        }
        if (!dram_rd_rsp_active_) {
            if (!mem_req_queue_.empty() && mem_req_queue_.front()->ready &&
                !mem_req_queue_.front()->write) {
                auto& mreq = mem_req_queue_.front();
                dut_->ram_bus___05Fr___05Fvalid = 1;
                dut_->ram_bus___05Fr___05Fbits___05Fid = mreq->tag;
                dut_->ram_bus___05Fr___05Fbits___05Fresp = 0;
                dut_->ram_bus___05Fr___05Fbits___05Flast =
                    mreq->beat == MEM_BLOCK_BEATS - 1;
                dut_->ram_bus___05Fr___05Fbits___05Fdata =
                    mreq->data[mreq->beat++];

                if (mreq->beat == MEM_BLOCK_BEATS) {
                    mem_req_queue_.pop_front();
                }
                dram_rd_rsp_active_ = true;
            } else {
                dut_->ram_bus___05Fr___05Fvalid = 0;
            }
        }

        if (dram_wr_rsp_active_ && dut_->ram_bus___05Fb___05Fvalid &&
            dram_wr_rsp_ready_) {
            dram_wr_rsp_active_ = false;
        }
        if (!dram_wr_rsp_active_) {
            if (!mem_req_queue_.empty() && mem_req_queue_.front()->ready &&
                mem_req_queue_.front()->write) {
                auto& mreq = mem_req_queue_.front();
                dut_->ram_bus___05Fb___05Fvalid = 1;
                dut_->ram_bus___05Fb___05Fbits___05Fid = mreq->tag;
                dut_->ram_bus___05Fb___05Fbits___05Fresp = 0;

                mem_req_queue_.pop_front();
                dram_wr_rsp_active_ = true;
            } else {
                dut_->ram_bus___05Fb___05Fvalid = 0;
            }
        }

        if (dut_->ram_bus___05Faw___05Fvalid ||
            dut_->ram_bus___05Far___05Fvalid) {
            uint64_t req_addr = dut_->ram_bus___05Faw___05Fvalid
                                    ? dut_->ram_bus___05Faw___05Fbits___05Faddr
                                    : dut_->ram_bus___05Far___05Fbits___05Faddr;

            if (dut_->ram_bus___05Faw___05Fvalid) {
                auto mem_req = std::make_unique<MemoryRequest>(
                    req_addr, dut_->ram_bus___05Faw___05Fbits___05Fid, true);

                dram_write_queue_.emplace_back(std::move(mem_req));
            } else {
                auto mem_req = std::make_unique<MemoryRequest>(
                    req_addr, dut_->ram_bus___05Far___05Fbits___05Fid, false);
                memcpy(mem_req->data, &ram_->phys_mem[req_addr],
                       sizeof(mem_req->data));

                dram_queue_.emplace(
                    req_addr, ramulator::Request::Type::READ,
                    [mreq = mem_req.get()](ramulator::Request& dram_req) {
                        mreq->ready = true;
                    });

                mem_req_queue_.emplace_back(std::move(mem_req));
            }
        }

        dut_->ram_bus___05Fw___05Fready = 1;

        if (dut_->ram_bus___05Fw___05Fvalid) {
            if (!dram_write_queue_.empty()) {
                auto& mreq = dram_write_queue_.front();

                mreq->data[mreq->beat] =
                    dut_->ram_bus___05Fw___05Fbits___05Fdata;
                mreq->mask[mreq->beat] =
                    dut_->ram_bus___05Fw___05Fbits___05Fstrb;
                mreq->beat++;

                if (mreq->beat == MEM_BLOCK_BEATS) {
                    auto* mem = reinterpret_cast<uint64_t*>(
                        &ram_->phys_mem[mreq->addr]);

                    for (int i = 0; i < MEM_BLOCK_BEATS; i++) {
                        uint64_t wdata = 0;

                        for (int j = 0; j < 8; j++) {
                            if (mreq->mask[i] & (1 << j))
                                wdata |= mreq->data[i] & (0xff << (j * 8));
                            else
                                wdata |= mem[i] & (0xff << (j * 8));
                        }

                        mem[i] = wdata;
                    }

                    mreq->ready = true;
                    dram_queue_.emplace(mreq->addr,
                                        ramulator::Request::Type::WRITE, 0);

                    mem_req_queue_.emplace_back(
                        std::move(dram_write_queue_.front()));
                    dram_write_queue_.pop_front();
                }
            } else {
                dut_->ram_bus___05Fw___05Fready = 0;
            }
        }

        dut_->ram_bus___05Far___05Fready = 1;
        dut_->ram_bus___05Faw___05Fready = 1;
    }
#endif
};

template <> SoC* Singleton<SoC>::m_singleton = nullptr;

SoC::SoC(std::string_view sd_image, uint64_t memory_addr, size_t memory_size,
         std::string_view bootrom, std::string_view dtb,
         std::string_view trace_log)
    : impl_(
          new Impl(sd_image, memory_addr, memory_size, bootrom, dtb, trace_log))
{}

SoC::~SoC() { delete impl_; }

int SoC::run() { return impl_->run(); }

void SoC::memory_read(uint64_t addr, uint64_t* data, int log_size)
{
    impl_->memory_read(addr, data, log_size);
}

void SoC::memory_write(uint64_t addr, int byte_mask, uint64_t data,
                       int log_size)
{
    impl_->memory_write(addr, byte_mask, data, log_size);
}

} // namespace room

extern "C" void dpi_memory_read(uint64_t addr, uint64_t* data, int log_size)
{
    room::SoC::get_singleton().memory_read(addr, data, log_size);
}

extern "C" void dpi_memory_write(uint64_t addr, int byte_mask, uint64_t data,
                                 int log_size)
{
    room::SoC::get_singleton().memory_write(addr, byte_mask, data, log_size);
}
