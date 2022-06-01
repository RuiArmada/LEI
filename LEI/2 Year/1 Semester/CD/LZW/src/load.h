// Author: Rui Filipe Pimenta Armada
// Date: 2021-15-02

#ifndef LOAD_H
#define LOAD_H

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <limits.h>
#include <math.h>
#include <string.h>

#define BLOCK_SIZE  65536   /**< 64Kbytes */
#define DIC 1024

void array(uint8_t* file, int current,int end, uint8_t* source);

int comp_arrays(uint8_t* arr1,uint8_t* arr2);

void convert(uint16_t help, uint8_t* out);

void copy_arr(uint8_t* from, uint8_t* to);

void write_dic(uint8_t* from,uint8_t* plus, uint8_t* to);

void algorithm(uint8_t* file,uint8_t** dic, uint16_t* out);

int file_length(FILE *file);

int translate(uint16_t* in,char* out);

long double call_lzwd(char* file);

#endif // __LOAD_H 