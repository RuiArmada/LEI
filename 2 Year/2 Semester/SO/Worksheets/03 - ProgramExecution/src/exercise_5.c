#include "../include/include.h"

int main(int argc , char const *argv[]) {
    int status;
    for(int i = 1 ; i <= 10 ; i++) {
        pid_t pid = getpid();
        pid_t ppid = getppid();
        printf("Process %d \nPID = %d \nParentPID = %d\n\n", i, pid, ppid);
        if(fork()) {
            int teminated_pid = wait(&status);
            printf("Process %d has ended, exit code %d. \n", terminated_pid, WEXITSTATUS(status));
            _exit(i);
        }
    }
    return 0;
}