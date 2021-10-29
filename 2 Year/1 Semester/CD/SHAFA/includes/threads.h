#ifndef THREADS_H
#define THREADS_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>
#include "error.h"

#ifdef _WIN32
#define THREADS
#define WIN_THREAD

#elif defined(__unix__) || defined(__APPLE__) || defined(__MACH__)
#define THREADS
#define POSX_THREAD

#else
#define _NO_THREAD
#endif

extern bool NO_THREAD;

typedef enum {
    STOP,
    START
} CLOCK;

float clock_thread(CLOCK action);

ERROR thread_create(ERROR (* process)(void *), ERROR (* write)(void *, ERROR, ERROR), void * args);

ERROR thread_wait();

#endif