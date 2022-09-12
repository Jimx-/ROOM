#include "generated/platform.h"
#include "serial.h"
#include "mmio.h"
#include "utils.h"

#include <stddef.h>

#define CMD0   (0) /* GO_IDLE_STATE */
#define CMD1   (1) /* SEND_OP_COND */
#define CMD2   (2) /* SEND_CID */
#define CMD3   (3) /* RELATIVE_ADDR */
#define CMD4   (4)
#define CMD5   (5)  /* SLEEP_WAKE (SDC) */
#define CMD6   (6)  /* SWITCH_FUNC */
#define CMD7   (7)  /* SELECT */
#define CMD8   (8)  /* SEND_IF_COND */
#define CMD9   (9)  /* SEND_CSD */
#define CMD10  (10) /* SEND_CID */
#define CMD11  (11)
#define CMD12  (12) /* STOP_TRANSMISSION */
#define CMD13  (13)
#define CMD15  (15)
#define CMD16  (16) /* SET_BLOCKLEN */
#define CMD17  (17) /* READ_SINGLE_BLOCK */
#define CMD18  (18) /* READ_MULTIPLE_BLOCK */
#define CMD19  (19)
#define CMD20  (20)
#define CMD23  (23)
#define CMD24  (24)
#define CMD25  (25)
#define CMD27  (27)
#define CMD28  (28)
#define CMD29  (29)
#define CMD30  (30)
#define CMD32  (32)
#define CMD33  (33)
#define CMD38  (38)
#define CMD42  (42)
#define CMD55  (55) /* APP_CMD */
#define CMD56  (56)
#define ACMD6  (0x80 + 6)  /* define the data bus width */
#define ACMD41 (0x80 + 41) /* SEND_OP_COND (ACMD) */

#define SDC_CTRL_SD_4BIT  0x1
#define SDC_CTRL_SD_RESET 0x2

#define SDC_ISR_CC 0x1

#define send_cmd(cmd, arg, resp) send_data_cmd((cmd), (arg), (resp), NULL, 0)

static int finish_cmd(unsigned int cmd, unsigned int response[4])
{
    unsigned int status;

    for (;;) {
        status = mmio_read32((void*)SDC_CMD_ISR_ADDRESS);

        if (status) {
            mmio_write32((void*)SDC_CMD_ISR_ADDRESS, 0);

            if (status == SDC_ISR_CC) {
                if (response) {
                    response[0] = mmio_read32((void*)SDC_RESP0_ADDRESS);
                    response[1] = mmio_read32((void*)SDC_RESP1_ADDRESS);
                    response[2] = mmio_read32((void*)SDC_RESP2_ADDRESS);
                    response[3] = mmio_read32((void*)SDC_RESP3_ADDRESS);
                }

                return 0;
            }

            break;
        }
    }

    return -1;
}

static int finish_data(void* buf, unsigned int blocks)
{
    unsigned int status;

    for (;;) {
        status = mmio_read32((void*)SDC_DATA_ISR_ADDRESS);

        if (status) {
            mmio_write32((void*)SDC_DATA_ISR_ADDRESS, 0);

            if (status & SDC_ISR_CC) {
                unsigned int data;
                int i;

                for (i = 0; i < (blocks << 7); i++) {
                    data = mmio_read32((void*)SDC_BUFFER_ADDRESS);
                    *(unsigned int*)(buf + (i << 2)) = data;
                }
            }

            if (status == SDC_ISR_CC) return 0;

            break;
        }
    }

    return -1;
}

static int send_data_cmd(unsigned int cmd, unsigned int arg,
                         unsigned int response[4], void* buf,
                         unsigned int blocks)
{
    unsigned int command = (cmd & 0x3f) << 8;

    switch (cmd) {
    case CMD0:
    case CMD4:
    case CMD15:
        // No responce
        break;
    case CMD11:
    case CMD13:
    case CMD16:
    case CMD17:
    case CMD18:
    case CMD19:
    case CMD23:
    case CMD24:
    case CMD25:
    case CMD27:
    case CMD30:
    case CMD32:
    case CMD33:
    case CMD42:
    case CMD55:
    case CMD56:
    case ACMD6:
        // R1
        command |= 1;      // 48 bits
        command |= 1 << 3; // resp CRC
        command |= 1 << 4; // resp OPCODE
        break;
    case CMD7:
    case CMD12:
    case CMD20:
    case CMD28:
    case CMD29:
    case CMD38:
        // R1b
        command |= 1;      // 48 bits
        command |= 1 << 2; // busy
        command |= 1 << 3; // resp CRC
        command |= 1 << 4; // resp OPCODE
        break;
    case CMD2:
    case CMD9:
    case CMD10:
        // R2
        command |= 2;      // 136 bits
        command |= 1 << 3; // resp CRC
        break;
    case ACMD41:
        // R3
        command |= 1; // 48 bits
        break;
    case CMD3:
        // R6
        command |= 1;      // 48 bits
        command |= 1 << 2; // busy
        command |= 1 << 3; // resp CRC
        command |= 1 << 4; // resp OPCODE
        break;
    case CMD8:
        // R7
        command |= 1;      // 48 bits
        command |= 1 << 3; // resp CRC
        command |= 1 << 4; // resp OPCODE
        break;
    }

    if (blocks) {
        command |= 1 << 5;
        if ((intptr_t)buf & 3) {
            return -1;
        }

        mmio_write32((void*)SDC_BLKSIZE_ADDRESS, 511);
        mmio_write32((void*)SDC_BLKCNT_ADDRESS, blocks - 1);
    }

    mmio_write32((void*)SDC_COMMAND_ADDRESS, command);
    mmio_write32((void*)SDC_ARGUMENT_ADDRESS, arg);

    if (finish_cmd(cmd, response) < 0) return -1;
    if (blocks) return finish_data(buf, blocks);

    return 0;
}

int sd_init(void)
{
    unsigned int ctrl;
    unsigned int response[4];
    unsigned int rca;

    /* SD controller soft reset */
    mmio_write32((void*)SDC_RESET_ADDRESS, 1);
    mmio_write32((void*)SDC_DIVISOR_ADDRESS, 0x7c);
    mmio_write32((void*)SDC_RESET_ADDRESS, 0);
    usleep(5000);

    /* Reset SD card */
    ctrl = mmio_read32((void*)SDC_CTRL_SETTING_ADDRESS);
    ctrl |= SDC_CTRL_SD_RESET;
    mmio_write32((void*)SDC_CTRL_SETTING_ADDRESS, ctrl);
    usleep(1000000);
    ctrl &= ~SDC_CTRL_SD_RESET;
    mmio_write32((void*)SDC_CTRL_SETTING_ADDRESS, ctrl);
    usleep(100000);

    /* Go idle */
    send_cmd(CMD0, 0, NULL);

    if (send_cmd(CMD8, 0x1AA, response) == 0) {
        if ((response[0] & 0xfff) != 0x1AA) {
            return -1;
        }
    }

    while (1) {
        if (send_cmd(CMD55, 0, NULL) < 0 ||
            send_cmd(ACMD41, 0x40300000, response) < 0)
            return -1;
        if (response[0] & (1 << 31)) {
            break;
        }
    }

    if (send_cmd(CMD2, 0, response) < 0) return -1;

    rca = 0x1234;
    if (send_cmd(CMD3, rca << 16, response) < 0) return -1;
    rca = response[0] >> 16;

    /* Select card */
    if (send_cmd(CMD7, rca << 16, NULL) < 0) return -1;

    /* 12.5 MHz */
    mmio_write32((void*)SDC_DIVISOR_ADDRESS, 4);
    usleep(10000);

    /* 4-bit mode */
    ctrl = mmio_read32((void*)SDC_CTRL_SETTING_ADDRESS);
    ctrl |= SDC_CTRL_SD_4BIT;
    mmio_write32((void*)SDC_CTRL_SETTING_ADDRESS, ctrl);
    if (send_cmd(CMD55, rca << 16, NULL) < 0 || send_cmd(ACMD6, 2, NULL) < 0)
        return -1;

    /* Set block length */
    if (send_cmd(CMD16, 512, NULL) < 0) return -1;

    return 0;
}
