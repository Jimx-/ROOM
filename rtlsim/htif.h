#ifndef _RTLSIM_HTIF_H_
#define _RTLSIM_HTIF_H_

#include <cstdint>
#include <optional>

namespace room {

class Htif {
public:
    explicit Htif(uint64_t tohost, uint64_t fromhost)
        : tohost_(tohost), fromhost_(fromhost)
    {}

    bool write(uint64_t addr, uint64_t data);

    bool should_stop() const { return exitcode_.has_value(); }

private:
    class Command {
    public:
        explicit Command(uint64_t tohost) : tohost_(tohost) {}

        uint8_t device() const { return tohost_ >> 56; }
        uint8_t cmd() const { return tohost_ >> 48; }
        uint64_t payload() const { return tohost_ << 16 >> 16; }

    private:
        uint64_t tohost_;
    };

    void handle_command(const Command& cmd);

    uint64_t tohost_;
    uint64_t fromhost_;
    std::optional<uint64_t> exitcode_;
};

} // namespace room

#endif
