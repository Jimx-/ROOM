#include "sdcard.h"

#include <iostream>
#include <fstream>
#include "spdlog/spdlog.h"

#define MMC_RSP_PRESENT (1 << 0)
#define MMC_RSP_136     (1 << 1)
#define MMC_RSP_CRC     (1 << 2)
#define MMC_RSP_BUSY    (1 << 3)
#define MMC_RSP_OPCODE  (1 << 4)

#define MMC_RSP_NONE (0)
#define MMC_RSP_R1   (MMC_RSP_PRESENT | MMC_RSP_CRC | MMC_RSP_OPCODE)
#define MMC_RSP_R1B \
    (MMC_RSP_PRESENT | MMC_RSP_CRC | MMC_RSP_OPCODE | MMC_RSP_BUSY)
#define MMC_RSP_R2 (MMC_RSP_PRESENT | MMC_RSP_136 | MMC_RSP_CRC)
#define MMC_RSP_R3 (MMC_RSP_PRESENT)
#define MMC_RSP_R4 (MMC_RSP_PRESENT)
#define MMC_RSP_R5 (MMC_RSP_PRESENT | MMC_RSP_CRC | MMC_RSP_OPCODE)
#define MMC_RSP_R6 (MMC_RSP_PRESENT | MMC_RSP_CRC | MMC_RSP_OPCODE)
#define MMC_RSP_R7 (MMC_RSP_PRESENT | MMC_RSP_CRC | MMC_RSP_OPCODE)

namespace room {

SDCard::SDCard(std::string_view image)
    : filename_(image), last_sdio_cmd_o_(1), last_sdio_data_o_(0xf),
      cmd_state_(CommandState::RESET), resp_type_(0),
      data_state_(DataState::IDLE)
{
    image_.open(filename_.c_str(),
                std::ios::in | std::ios::out | std::ios::binary);
    if (!image_.is_open()) {
        spdlog::error("Failed to open SD image {}", image);
        throw std::runtime_error("Failed to open SD image");
    }

    data_buf_ = std::make_unique<unsigned char[]>(512);

    card_status_ = 0x120;
}

SDCard::~SDCard()
{
    if (image_.is_open()) image_.close();
}

std::tuple<int, int> SDCard::tick(int sdio_clk, int sdio_cmd_i, int sdio_cmd_t,
                                  int sdio_data_i)
{
    int clk_posedge = sdio_clk & !last_clk_;

    last_clk_ = sdio_clk;

    if (clk_posedge) {
        last_sdio_cmd_o_ = command_tick(sdio_cmd_i, sdio_cmd_t);
        last_sdio_data_o_ = data_tick(sdio_data_i);
    }

    return std::make_tuple(last_sdio_cmd_o_, last_sdio_data_o_);
}

int SDCard::command_tick(int sdio_cmd_i, int sdio_cmd_t)
{
    int sdio_cmd_o = 1;

    switch (cmd_state_) {
    case CommandState::RESET:
        if (sdio_cmd_i && !sdio_cmd_t) cmd_state_ = CommandState::IDLE;

        break;
    case CommandState::IDLE:
        if (!sdio_cmd_i && !sdio_cmd_t) {
            cmd_state_ = CommandState::READING;
            cmd_count_ = 47;
            cmd_data_ = 0;
        } else if (resp_type_) {
            cmd_state_ = CommandState::WRITING;
            resp_crc_ = 0;
            resp_count_ = (resp_type_ & MMC_RSP_136) ? 135 : 47;
        }

        break;
    case CommandState::READING:
        if (cmd_count_) {
            cmd_data_ = (sdio_cmd_i & 1) | (cmd_data_ << 1);
            cmd_count_--;
        } else {
            handle_command();
            cmd_state_ = CommandState::IDLE;
        }
        break;
    case CommandState::WRITING:
        if (resp_type_ & MMC_RSP_136) {
            if (resp_count_ >= 134) { /* Start & transmission bit */
                sdio_cmd_o = 0;
            } else if (resp_count_ >= 128 && resp_count_ < 134) { /* Reserved */
                sdio_cmd_o = 1;
            } else if (resp_count_ >= 8 && resp_count_ < 128) { /* CSD/CID */
                sdio_cmd_o =
                    (resp_r2_[(resp_count_ >> 6) & 1] >> (resp_count_ & 0x3f)) &
                    1;
            } else if (resp_count_ >= 1 && resp_count_ < 8) { /* CRC */
                sdio_cmd_o = (resp_type_ & MMC_RSP_CRC)
                                 ? ((resp_crc_ >> (resp_count_ - 1)) & 1)
                                 : 1;
            }

            if ((resp_type_ & MMC_RSP_CRC) && resp_count_ >= 8 &&
                resp_count_ < 128)
                update_resp_crc(sdio_cmd_o);
        } else {
            if (resp_count_ >= 46) { /* Start & transmission bit */
                sdio_cmd_o = 0;
            } else if (resp_count_ >= 40 &&
                       resp_count_ < 46) { /* Command index */
                sdio_cmd_o = (resp_type_ & MMC_RSP_OPCODE)
                                 ? ((resp_opcode_ >> (resp_count_ - 40)) & 1)
                                 : 1;
            } else if (resp_count_ >= 8 && resp_count_ < 40) { /* Card status */
                sdio_cmd_o = (resp_status_ >> (resp_count_ - 8)) & 1;
            } else if (resp_count_ >= 1 && resp_count_ < 8) { /* CRC */
                sdio_cmd_o = (resp_type_ & MMC_RSP_CRC)
                                 ? ((resp_crc_ >> (resp_count_ - 1)) & 1)
                                 : 1;
            }

            if ((resp_type_ & MMC_RSP_CRC) && resp_count_ >= 8 &&
                resp_count_ < 47)
                update_resp_crc(sdio_cmd_o);
        }

        if (resp_count_ == 0) {
            resp_type_ = 0;
            cmd_state_ = CommandState::IDLE;
        } else
            resp_count_--;

        break;
    }

    return sdio_cmd_o;
}

void SDCard::update_resp_crc(int bit)
{
    int inv;

    inv = bit ^ ((resp_crc_ >> 6) & 1);
    resp_crc_ = (resp_crc_ << 1) & 0x7f;
    resp_crc_ |= inv;
    resp_crc_ ^= (inv << 3);
}

void SDCard::handle_command()
{
    unsigned int argument;
    int command;

    argument = (cmd_data_ >> 8) & 0xffffffff;
    command = (cmd_data_ >> 40) & 0x3f;

    spdlog::debug("Process SD card command={} argument={:#x}", command,
                  argument);

    if (cmd_is_app_) {
        switch (command) {
        case 6:
            handle_acmd6(argument);
            break;
        case 41:
            handle_acmd41(argument);
            break;
        case 51:
            handle_acmd51(argument);
            break;
        }

        cmd_is_app_ = false;
    } else {
        switch (command) {
        case 2:
            handle_cmd2();
            break;
        case 3:
            handle_cmd3();
            break;
        case 7:
            handle_cmd7(argument);
            break;
        case 8:
            handle_cmd8(argument);
            break;
        case 9:
            handle_cmd9(argument);
            break;
        case 16:
            handle_cmd16(argument);
            break;
        case 17:
            handle_cmd17(argument);
            break;
        case 55:
            handle_cmd55(argument);
            break;
        }
    }

    if (resp_type_ & MMC_RSP_OPCODE) resp_opcode_ = command;
}

void SDCard::handle_cmd2()
{
    resp_type_ = MMC_RSP_R2;
    resp_r2_[0] = 0;
    resp_r2_[1] = 0;
}

void SDCard::handle_cmd3()
{
    resp_type_ = MMC_RSP_R6;
    resp_status_ = 1 << 16;
}

void SDCard::handle_cmd7(uint32_t arg)
{
    resp_type_ = MMC_RSP_R1;
    resp_status_ = 0;
}

void SDCard::handle_cmd8(uint32_t arg)
{
    resp_type_ = MMC_RSP_R1;
    resp_status_ = 0x1aa;
}

void SDCard::handle_cmd9(uint32_t arg)
{
    resp_type_ = MMC_RSP_R2;
    resp_r2_[0] = 0x5b590001400e0032UL;
    resp_r2_[1] = 0xa404000dbd37f80UL;
}

void SDCard::handle_cmd16(uint32_t arg)
{
    block_len = arg;
    resp_type_ = MMC_RSP_R1;
    resp_status_ = 0x1aa;
}

void SDCard::handle_cmd55(uint32_t arg)
{
    resp_type_ = MMC_RSP_R1;
    resp_status_ = card_status_;
    cmd_is_app_ = true;
}

void SDCard::handle_acmd6(uint32_t arg)
{
    bus_4bit = (arg & 3) == 2;
    resp_type_ = MMC_RSP_R1;
    resp_status_ = 0;
}

void SDCard::handle_acmd41(uint32_t arg)
{
    resp_type_ = MMC_RSP_R3;
    resp_status_ = 0x40ff8000 | (arg == 0 ? 0 : (1UL << 31));
}

void SDCard::handle_acmd51(uint32_t arg)
{
    resp_type_ = MMC_RSP_R1;
    resp_status_ = 0;

    data_buf_[0] = 0x02;
    data_buf_[1] = 0x45;
    data_buf_[2] = 0x80;
    data_buf_[3] = 0x43;
    data_buf_[4] = data_buf_[5] = data_buf_[6] = data_buf_[7] = 0;
    data_count_ = 8;
}

void SDCard::handle_cmd17(uint32_t arg)
{
    resp_type_ = MMC_RSP_R1;
    resp_status_ = 0;

    image_.seekg(arg * 512, std::ios::beg);
    image_.read((char*)data_buf_.get(), 512);
    data_count_ = 512;
}

int SDCard::data_tick(int sdio_data_i)
{
    int sdio_data_o = 0xf;

    switch (data_state_) {
    case DataState::IDLE:
        if (data_count_ > 0) {
            spdlog::debug("Start transmitting SD data size={}", data_count_);

            if (bus_4bit) {
                data_xfr_count_ = (data_count_ << 1) + 16 + 1;
            } else {
                data_xfr_count_ = (data_count_ << 3) + 16 + 1;
            }

            memset(data_crc_, 0, sizeof(data_crc_));

            data_state_ = DataState::WRITING;
        }
        break;
    case DataState::WRITING:
        if (bus_4bit) {
            if (data_xfr_count_ ==
                (data_count_ << 1) + 16 + 1) { /* Start bit */
                sdio_data_o = 0;
            } else if (data_xfr_count_ >= 17 &&
                       data_xfr_count_ <
                           (data_count_ << 1) + 16 + 1) { /* Data */
                int data_index =
                    data_count_ - ((data_xfr_count_ - 17) >> 1) - 1;

                sdio_data_o = (data_buf_[data_index] >>
                               (((data_xfr_count_ - 17) & 1) << 2)) &
                              0xf;

                for (int i = 0; i < 4; i++) {
                    update_data_crc(data_crc_[i], (sdio_data_o >> i) & 1);
                }
            } else if (data_xfr_count_ >= 1 && data_xfr_count_ < 17) { /* CRC */
                sdio_data_o = 0;
                for (int i = 0; i < 4; i++) {
                    sdio_data_o |= ((data_crc_[i] >> (data_xfr_count_ - 1)) & 1)
                                   << i;
                }
            }
        } else {
            if (data_xfr_count_ ==
                (data_count_ << 3) + 16 + 1) { /* Start bit */
                sdio_data_o = 0;
            } else if (data_xfr_count_ >= 17 &&
                       data_xfr_count_ <
                           (data_count_ << 3) + 16 + 1) { /* Data */
                int data_index = data_xfr_count_ - 17;

                sdio_data_o = (data_buf_[data_count_ - (data_index >> 3) - 1] >>
                               (data_index & 0x7)) &
                              1;

                update_data_crc(data_crc_[0], sdio_data_o);
            } else if (data_xfr_count_ >= 1 && data_xfr_count_ < 17) { /* CRC */
                sdio_data_o = (data_crc_[0] >> (data_xfr_count_ - 1)) & 1;
            }
        }

        if (data_xfr_count_ == 0) {
            data_count_ = 0;
            data_state_ = DataState::IDLE;
        } else
            data_xfr_count_--;

        break;
    }

    return sdio_data_o;
}

void SDCard::update_data_crc(int& crc, int bit)
{
    int inv;

    inv = bit ^ ((crc >> 15) & 1);
    crc = (crc << 1) & 0xffff;
    crc |= inv;
    crc ^= (inv << 5);
    crc ^= (inv << 12);
}

} // namespace room
