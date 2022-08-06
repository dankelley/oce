// https://en.cppreference.com/w/cpp/types/integer
#include <stdio.h>
#include <stdlib.h>

short ChecksumORIG(short *phBuff, int n) {
    int i;
    short hChecksum = 0xb58c;
    for (i=0; i<n; i++)
        hChecksum += phBuff[i];
    return hChecksum;
}

unsigned short cs_oce(unsigned char *data, unsigned short size, int debug)
{
  unsigned short checksum = 0xB58C;
  if (debug > 1) {
      printf("    %d data: 0x%02x 0x%02x 0x%02x 0x%02x ... 0x%02x 0x%02x 0x%02x 0x%02x\n",
              size, data[0], data[1], data[2], data[3],
              data[size-4], data[size-3], data[size-2], data[size-1]);
  }
  for (int i = 0; i < size; i += 2) {
    checksum += (unsigned short)data[i] + 256*(unsigned short)data[i+1];
  }
  if (1 == size%2) {
    if (debug > 1) {
      printf("    odd # data, so cs changed from 0x%x ", checksum);
    }
    checksum += 256*(unsigned short)data[size-1];
    if (debug > 1) {
      printf("to 0x%x\n", checksum);
    }
  }
  return(checksum);
}
// This follows the pattern of Page 79 of Ref 3, but with specified types, to
// avoid possibly problems porting the nortek code, which has unstated
// assumptions (on endianness, on the size of a 'short', etc.
int16_t nortek_checksum(uint_fast16_t *phBuff, int n) {
    uint_fast32_t hChecksum = 0xb58c; // I think this is large enough
    uint_fast16_t res;
    printf("checksum(buffer, n=%d)\n", n);
    printf(" start with 0x%04x\n", hChecksum);
    for (int i=0; i<n; i++) {
        hChecksum += phBuff[i];
        printf(" add 0x%04x to get 0x%08x\n", phBuff[i], hChecksum);
    }
    res = (uint_fast16_t) (hChecksum % 0xffff);
    return res;
}


int main(int argc, char **argv)
{
    FILE *in;
    int32_t len;
    unsigned char *buf;
    uint16_t *buf2;
    uint16_t cs2, cs3;
    if (argc != 2) {
        printf("ERROR: give a filename\n");
        exit(1);
    }
    in = fopen(argv[1], "rb");
    fseek(in, 0, SEEK_END);
    len = ftell(in);
    rewind(in);
    buf = (unsigned char*)malloc(len*sizeof(unsigned char));
    fread(buf, 1, len, in);
    printf("buf[0:%d]: ", len-1);
    for (int32_t i = 0; i < len; i++)
        printf("0x%x ", buf[i]);
    printf("\n");
    // read 2 bytes at a time
    rewind(in);
    buf2 = (uint16_t*)malloc(len/2*sizeof(uint16_t));
    fread(buf2, 2, len/2, in);
    cs2 = nortek_checksum(buf2, len/2);
    printf("computed checksum cs2: 0x%04x (expect 0xb4e0)\n", cs2);
    cs3 = cs_oce(buf, len, 0);
    printf("computed checksum cs3: 0x%04x (expect 0xb4e0)\n", cs3);
    fclose(in);
}

