#include "../include/includes.h"

int main(int argc , char const *argv[]) {
    char buff[1024];
    ssize_t b_read = 0;
    while(b_read = read(0 , buff , 1024)) 
        write(STDOUT_FILENO , buff , b_read);
    return 0;
}