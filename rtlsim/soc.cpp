#include "soc.h"

#include <memory>
#include <iostream>

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

        register_ram(memory_addr, (uint64_t)memory_size);

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
    }

    ~Impl()
    {
#if defined(VCD_OUTPUT) || defined(FST_OUTPUT)
        trace_->close();
        delete trace_;
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

    struct PhysMemoryRange {
        uint64_t addr;
        uint64_t size;
        bool is_ram;
        std::unique_ptr<uint8_t[]> phys_mem;
    };

    std::vector<PhysMemoryRange> mem_map_;

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

    void reset()
    {
        dut_->rst = 1;

        for (int i = 0; i < RESET_DELAY; i++) {
            dut_->clk = 0;
            this->eval();
            dut_->clk = 1;
            this->eval();
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
        this->eval();

        dut_->clk = 1;
        this->eval();
    }

    void eval()
    {
        dut_->eval();
#if defined(VCD_OUTPUT) || defined(FST_OUTPUT)
        if (sim_trace_enabled()) {
            trace_->dump(timestamp);
        }
#endif
        ++timestamp;
    }
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
