#ifndef _RTLSIM_SOC_H_
#define _RTLSIM_SOC_H_

#include <string>

namespace room {

class SoC {
public:
    SoC(std::string_view sd_image);
    ~SoC();

    int run();

private:
    class Impl;
    Impl* impl_;
};

} // namespace room

#endif