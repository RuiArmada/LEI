#ifndef MODULED_H
#define MODULED_H

#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>


#include "file.h"
#include "auxiliar.h"
#include "threads.h"
#include "error.h"

#define SYMBOLS 256

typedef enum {
    
    _RLE,
    _SHAFA,
    _SHAFA_RLE,

} ALGORITHM;

typedef struct {
    
    FILE* f_rle;
    FILE* f_write;
    unsigned long rle_size;
    unsigned long* final_size;
    uint8_t* buffer;
    
    uint8_t* seq;
} ARGUMENTS_RLE;

typedef struct btree {
    
    char symbol;
    struct btree *left, *right;

} *BTREE;

typedef struct {

    FILE* f_write;
    char* code_ID;
    unsigned long* rle_size;
    unsigned long* final_size;
    uint8_t* rle_decomp;
    uint8_t shafa_decomp;
    uint8_t shafa_code;
    bool rle_decompress;

} ARGUMENTS_SHAFA;

ERROR decompress_SHAFA(char** path, bool decompress_rle);

ERROR decompress_RLE(char** path);

#endif