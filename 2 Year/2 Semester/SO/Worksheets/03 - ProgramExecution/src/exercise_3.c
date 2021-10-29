#include "../include/include.h"

int main(int argc , char const *argv[]) {
    for(int i = 1 ; i < 11 ; i++) {
        if(fork() == 0) {
            pid_t PID = getpid();
            pid_t PPID = getppid();
            printf("Child PID - %d \n Child Parent PID - %d \n", PID, PPID);
            _exit(i);
        }
        int exit;
        wait(&exit);
        printf("%d \n", WEXITSTATUS(exit));
    }
    return 0;
}