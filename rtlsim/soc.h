#ifndef _RTLSIM_SOC_H_
#define _RTLSIM_SOC_H_

#include "singleton.h"

#include <cstdint>
#include <string>

namespace room {

class SoC : public Singleton<SoC> {
public:
    SoC(std::string_view sd_image, uint64_t memory_addr, size_t memory_size,
        std::string_view bootrom, std::string_view dtb, std::string_view initrd,
        off_t initrd_offset, std::string_view trace_log, bool enable_fork,
        size_t fork_interval, std::string_view sig_file, int sig_line_size);
    ~SoC();

    int run();

    void memory_read(uint64_t addr, uint64_t* data, int log_size);
    void memory_write(uint64_t addr, int byte_mask, uint64_t data,
                      int log_size);

private:
    class Impl;
    Impl* impl_;
};

} // namespace room

#endif
