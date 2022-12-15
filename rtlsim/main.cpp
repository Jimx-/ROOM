#include "soc.h"

#include "cxxopts.hpp"
#include "spdlog/cfg/env.h"
#include "spdlog/spdlog.h"

using cxxopts::OptionException;

cxxopts::ParseResult parse_arguments(int argc, char* argv[])
{
    try {
        cxxopts::Options options(argv[0], " - ROOM SoC simulation");

        // clang-format off
        options.add_options()
            ("sd-image", "SD card image", cxxopts::value<std::string>()->default_value(""))
            ("M,memory-size", "Memory size (MB)", cxxopts::value<unsigned int>()->default_value("256"))
            ("A,memory-addr", "Memory base address", cxxopts::value<uint64_t>()->default_value("0x80000000"))
            ("b,bootrom", "Path to boot ROM", cxxopts::value<std::string>()->default_value(""))
            ("h,help", "Print help");
        // clang-format on

        options.parse_positional({"sd-image"});

        auto result = options.parse(argc, argv);

        if (result.count("help")) {
            std::cerr << options.help() << std::endl;
            exit(EXIT_SUCCESS);
        }

        return result;
    } catch (const OptionException& e) {
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

    try {
        sd_image = options["sd-image"].as<std::string>();
        memory_addr = options["memory-addr"].as<uint64_t>();
        memory_size_mb = options["memory-size"].as<unsigned int>();
        bootrom = options["bootrom"].as<std::string>();
    } catch (const OptionException& e) {
        spdlog::error("Failed to parse options: {}", e.what());
        exit(EXIT_FAILURE);
    }

    room::SoC soc(sd_image, memory_addr, (size_t)memory_size_mb << 20UL,
                  bootrom);

    try {
        soc.run();
    } catch (std::runtime_error& e) {
        spdlog::error("Exiting simulation due to runtime error: {}", e.what());
    }

    return 0;
}
