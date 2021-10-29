#ifndef VECTOR_H
#define VECTOR_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Constant Definition */
#define MAX 10
#define MAX_LINE 100

/* Global Variables */
char line[MAX_LINE];

void fill(int *vector , int size , int value);

int find(int *vector , int size , int value);

#endif