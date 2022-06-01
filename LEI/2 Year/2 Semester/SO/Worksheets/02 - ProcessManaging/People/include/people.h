#ifndef PEOPLE_H
#define PEOPLE_H

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>

typedef struct person {
    char name[100];
    int age;
}PERSON;

#endif