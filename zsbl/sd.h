#ifndef _ZSBL_SD_H_
#define _ZSBL_SD_H_

void sd_init(void);

int sd_read_sector(unsigned int sector, unsigned char* buffer,
                   unsigned int sector_count);
int sd_write_sector(unsigned int sector, unsigned char* buffer,
                    unsigned int sector_count);

#endif
