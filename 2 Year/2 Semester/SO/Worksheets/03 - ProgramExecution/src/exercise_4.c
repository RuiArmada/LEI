#include "../include/include.h"

int main(int argc , char const *argv[]) {
    for(int i = 1 ; i < 11 ; i++) {
        if(!fork()) {
            _exit(i);
        }
    }
    int exit;
    while(wait(&exit) != -1)
        printf("Child %d \n", WEXITSTATUS(exit));
    return 0;
}