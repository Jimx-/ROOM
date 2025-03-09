#include "soc.h"

#include "cxxopts.hpp"
#include "spdlog/cfg/env.h"
#include "spdlog/spdlog.h"

#include <iostream>

cxxopts::ParseResult parse_arguments(int argc, char* argv[])
{
    try {
        cxxopts::Options options(argv[0], " - ROOM SoC simulation");

        // clang-format off
        options.add_options()
            ("sd-image", "SD card image", cxxopts::value<std::string>()->default_value(""))
            ("M,memory-size", "Memory size (MB)", cxxopts::value<unsigned int>()->default_value("1024"))
            ("A,memory-addr", "Memory base address", cxxopts::value<uint64_t>()->default_value("0x80000000"))
            ("b,bootrom", "Path to boot ROM", cxxopts::value<std::string>()->default_value(""))
            ("d,dtb", "Path to device tree blob", cxxopts::value<std::string>()->default_value(""))
            ("r,initrd", "Path to init ramdisk", cxxopts::value<std::string>()->default_value(""))
            ("initrd-offset", "Offset of init ramdisk to memory base address", cxxopts::value<off_t>()->default_value("0x20000000"))
            ("t,trace", "Path to trace log file", cxxopts::value<std::string>()->default_value(""))
            ("f,fork", "Enable fork snapshot", cxxopts::value<bool>()->default_value("false"))
            ("I,interval", "Fork interval", cxxopts::value<size_t>()->default_value("10000"))
            ("h,help", "Print help");
        // clang-format on

        options.parse_positional({"sd-image"});

        auto result = options.parse(argc, argv);

        if (result.count("help")) {
            std::cerr << options.help() << std::endl;
            exit(EXIT_SUCCESS);
        }

        return result;
    } catch (const cxxopts::exceptions::exception& e) {
        spdlog::error("Failed to parse arguments: {}", e.what());
        exit(EXIT_FAILURE);
    }
}

int main(int argc, char* argv[])
{
    spdlog::cfg::load_env_levels();

    auto options = parse_arguments(argc, argv);

    std::string sd_image;
    uint64_t memory_addr;
    unsigned int memory_size_mb;
    std::string bootrom;
    std::string dtb;
    std::string initrd;
    off_t initrd_offset;
    std::string trace_log;
    bool enable_fork;
    size_t fork_interval;

    try {
        sd_image = options["sd-image"].as<std::string>();
        memory_addr = options["memory-addr"].as<uint64_t>();
        memory_size_mb = options["memory-size"].as<unsigned int>();
        bootrom = options["bootrom"].as<std::string>();
        dtb = options["dtb"].as<std::string>();
        initrd = options["initrd"].as<std::string>();
        initrd_offset = options["initrd-offset"].as<off_t>();
        trace_log = options["trace"].as<std::string>();
        enable_fork = options["fork"].as<bool>();
        fork_interval = options["interval"].as<size_t>();
    } catch (const cxxopts::exceptions::exception& e) {
        spdlog::error("Failed to parse options: {}", e.what());
        exit(EXIT_FAILURE);
    }

    room::SoC soc(sd_image, memory_addr, (size_t)memory_size_mb << 20UL,
                  bootrom, dtb, initrd, initrd_offset, trace_log, enable_fork,
                  fork_interval);

    try {
        soc.run();
    } catch (std::runtime_error& e) {
        spdlog::error("Exiting simulation due to runtime error: {}", e.what());
    }

    return 0;
}
