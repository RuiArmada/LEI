#include "../include/include.h"

int main(void) {
    pid_t pid = 0;
    if((pid = fork()) == 0)
        execlp("ls" , "-l" , (char*) NULL);
    return 0;
}