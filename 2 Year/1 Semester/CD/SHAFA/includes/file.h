#ifndef FILE_H
#define FILE_H

#include <stdio.h>
#include <stdlib.h>

#define FSIZE_DEFAULT_BLOCK_SIZE 524288         
#define FSIZE_MIN_BLOCK_SIZE 512               
#define FSIZE_MAX_BLOCK_SIZE 67108864           
#define FSIZE_MAX_NUMBER_OF_BLOCKS 4294967296   
#define FSIZE_MAX_SIZE_FSEEK 2147483648         
#define FSIZE_ERROR_BLOCK_SIZE -1               
#define FSIZE_ERROR_NUMBER_OF_BLOCKS -2         
#define FSIZE_ERROR_IN_FILE -3                  
#define FSIZE_ERROR_IN_FTELL -1L                

enum {
    _1KiB   = 1024,
    _64KiB  = 65536,
    _640KiB = 655360,
    _8MiB   = 8388608,
    _64MiB  = 67108864
};

long long fsize(FILE *fp_in, char *filename, unsigned long *block_size, long *size_last_block);

#endif