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
            ("dromajo-cfg", "Dromajo config file", cxxopts::value<std::string>()->default_value(""))
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
    std::string dromajo_cfg;

    try {
        sd_image = options["sd-image"].as<std::string>();
        dromajo_cfg = options["dromajo-cfg"].as<std::string>();
    } catch (const OptionException& e) {
        spdlog::error("Failed to parse options: {}", e.what());
        exit(EXIT_FAILURE);
    }

    room::SoC soc(sd_image, dromajo_cfg);

    soc.run();

    return 0;
}
