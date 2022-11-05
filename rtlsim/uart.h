#ifndef _RTLSIM_UART_H_
#define _RTLSIM_UART_H_

namespace room {

class Uart {
public:
    Uart();

    void setup(int divisor, int nbits);

    int tick(int i_tx);

private:
    enum class RxState {
        IDLE,
        DATA,
    };

    int nbits_;
    int divisor_;

    int rx_baudcounter_;
    int rx_busy_;
    unsigned int rx_data_;
    RxState rx_state_;
};

} // namespace room

#endif
