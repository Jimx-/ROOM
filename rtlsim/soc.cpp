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

using namespace room;

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
    Impl(std::string_view sd_image, std::string_view dromajo_cfg)
    {
        Verilated::randReset(VERILATOR_RESET_VALUE);
        Verilated::randSeed(50);

        // turn off assertion before reset
        Verilated::assertOn(false);

        dut_.reset(new Vsoc_wrapper());

        if (!sd_image.empty()) sdcard.reset(new SDCard(sd_image));

#ifdef ITRACE
        (void)new Tracer(dromajo_cfg);
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
            }

            this->tick();
        }

        return 0;
    }

private:
    std::unique_ptr<Vsoc_wrapper> dut_;
    Uart uart;
    std::unique_ptr<SDCard> sdcard;
#ifdef VCD_OUTPUT
    VerilatedVcdC* trace_;
#elif defined(FST_OUTPUT)
    VerilatedFstC* trace_;
#endif

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
        uart.tick(dut_->tx);

        if (sdcard) {
            std::tie(dut_->sdio_cmd_i, dut_->sdio_data_i) =
                sdcard->tick(dut_->sdio_clk, dut_->sdio_cmd_o, dut_->sdio_cmd_t,
                             dut_->sdio_data_o);
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

SoC::SoC(std::string_view sd_image, std::string_view dromajo_cfg)
    : impl_(new Impl(sd_image, dromajo_cfg))
{}

SoC::~SoC() { delete impl_; }

int SoC::run() { return impl_->run(); }
