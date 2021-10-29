#include "../include/include.h"

int main(int argc , const char* argv[]) {
    for(size_t i = 1 ; i < argc ; i++)
        if(!fork())
            execlp(argv[i] , argv[i] , NULL);
    for(size_t i = 1 ; i < argc ; i++)
        wait(NULL);
    return 0;
}