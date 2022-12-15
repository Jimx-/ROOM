#include "uart.h"

#include <iostream>

namespace room {

Uart::Uart()
{
    rx_state_ = RxState::IDLE;

    setup(0x1b2, 8);
}

void Uart::setup(int divisor, int nbits)
{
    divisor_ = divisor;
    nbits_ = nbits;
}

int Uart::tick(int i_tx)
{
    if (rx_state_ == RxState::IDLE) {
        if (!i_tx) {
            rx_state_ = RxState::DATA;
            rx_baudcounter_ = divisor_ - 1;
            rx_busy_ = 0;
            rx_data_ = 0;
        }
    } else if (rx_baudcounter_ <= 0) {
        if (rx_busy_ >= (1 << (nbits_ - 1))) {
            char c;

            rx_state_ = RxState::IDLE;

            c = (rx_data_ >> (32 - nbits_)) & 0xff;
            std::cout << std::unitbuf << c;
        } else {
            rx_busy_ = (rx_busy_ << 1) | 1;
            rx_data_ = ((i_tx & 1) << 31) | (rx_data_ >> 1);
        }

        rx_baudcounter_ = divisor_ - 1;
    } else {
        rx_baudcounter_--;
    }

    return 1;
}

} // namespace room
