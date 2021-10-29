#ifndef MODULEC_H
#define MODULEC_H

#include <time.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "error.h"
#include "auxiliar.h"
#include "threads.h"

#define MAX_CODE_INT 32
#define NUM_SYMB 256
#define OFFSET 8

typedef struct {
    int index, next;
    uint8_t code[MAX_CODE_INT + 1];
} CODE_INDEX;

typedef struct {
    unsigned long block_size;
    FILE* f_SHAFA;
    char* codes;
    uint8_t* input;
    uint8_t* output;
    unsigned long* new_size;
} ARGS;

ERROR shafa(char** path);

#endif