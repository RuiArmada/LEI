#ifndef ERROR_H
#define ERROR_H

typedef enum errors {
    _SUCCESS                   = 0,
    _OUTSIDE                   = 1,
    _NO_MEMORY                 = 2,
    _FILE_INACCESSIBLE         = 3,
    _FILE_UNRECOGNIZABLE       = 4,
    _FILE_STREAM_FAILED        = 5,
    _FILE_TOO_SMALL            = 6,
    _THREAD_CREATION_FAILED    = 7,  
    _THREAD_TERMINATION_FAILED = 8,
} ERROR;

char * hades_prtcl(int num);

#endif