#ifndef READLN_H
#define READLN_H

#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <time.h>

typedef ssize_t (*funcs[2])(int fd , char* line , size_t size);

funcs readlns = {
    readln,
    readln2
};

ssize_t readln(int fd , char* line , size_t size);
ssize_t readln2(int fd , char* line , size_t size);

#endif