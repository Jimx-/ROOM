#include "htif.h"

#include <spdlog/spdlog.h>

namespace room {

bool Htif::write(uint64_t addr, uint64_t data, int log_size)
{
    if (addr != tohost_) return false;

    Command cmd(data);
    handle_command(cmd);

    return true;
}

void Htif::handle_command(const Command& cmd)
{
    if (cmd.device() == 0 && (cmd.payload() & 1)) {
        exitcode_ = cmd.payload();

        if (exit_code()) {
            spdlog::error("HTIF exited with failure (tohost = {})",
                          exit_code());
        }
    }
}

} // namespace room
