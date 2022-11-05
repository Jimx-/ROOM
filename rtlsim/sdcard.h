#ifndef _RTLSIM_SDCARD_H_
#define _RTLSIM_SDCARD_H_

#include <cstdint>
#include <string>
#include <fstream>
#include <memory>

namespace room {

class SDCard {
public:
    SDCard(std::string_view image);
    ~SDCard();

    std::tuple<int, int> tick(int sdio_clk, int sdio_cmd_i, int sdio_cmd_t,
                              int sdio_data_i);

private:
    enum class CommandState {
        RESET,
        IDLE,
        READING,
        WRITING,
    };

    enum class DataState {
        IDLE,
        READING,
        WRITING,
    };

    std::string filename_;
    std::fstream image_;

    int last_clk_;
    int last_sdio_cmd_o_;
    int last_sdio_data_o_;

    uint32_t card_status_;
    bool bus_4bit;
    int block_len;

    CommandState cmd_state_;
    int cmd_count_;
    uint64_t cmd_data_;
    bool cmd_is_app_;

    int resp_type_;
    int resp_opcode_;
    int resp_crc_;
    uint32_t resp_status_;
    uint64_t resp_r2_[2];
    int resp_count_;

    DataState data_state_;
    std::unique_ptr<char[]> data_buf_;
    int data_count_;
    int data_xfr_count_;
    int data_crc_[4];

    void update_resp_crc(int bit);
    void update_data_crc(int& crc, int bit);

    int command_tick(int sdio_cmd_i, int sdio_cmd_t);
    void handle_command();

    int data_tick(int sdio_data_i);

    void handle_cmd2();
    void handle_cmd3();
    void handle_cmd7(uint32_t arg);
    void handle_cmd8(uint32_t arg);
    void handle_cmd16(uint32_t arg);
    void handle_cmd17(uint32_t arg);
    void handle_cmd55(uint32_t arg);

    void handle_acmd6(uint32_t arg);
    void handle_acmd41(uint32_t arg);
};

} // namespace room

#endif
