#include "../include/readln.h"

ssize_t readln(int fd , char* line , size_t size) {
    ssize_t i = 0;
    while(i < size - 1) {
        ssize_t b_read = read(fd , &line[i] , 1);
        if(b_read < 1)
            break;
        if(line[i++] == '\n')
            break;
    }
    line[i] = 0;
    return i;
}

ssize_t readln2(int fd , char* line , size_t size) {
    ssize_t b_read = read(fd , line , size);
    if(!b_read)
        return 0;
    size_t l_length = strcspn(line , "\n") + 1;
    if(b_read < l_length)
        l_length = b_read;
    line[l_length] = 0;
    lseek(fd , l_length - b_read , SEEK_CUR);
    return l_length;
}