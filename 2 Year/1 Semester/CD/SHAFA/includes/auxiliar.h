#ifndef AUXILIAR_H
#define AUXILIAR_H

#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>

#define RLE ".rle"
#define FREQ ".freq"
#define CODES ".cod"
#define SHAFA ".shaf"

bool check_extention(char* path, char* ext);

char* add_extention(char* path, char* ext);

char* rm_extention(char* path);

#endif