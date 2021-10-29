#include "../include/include.h"

int main(int argc , char const *aegv[]) {
    pid_t pid;
    if((pid = fork()) == 0) {
        pid_t child_PID = getpid();
        pid_t child_PPID = getpid();
        printf("Child PID - %d \n Child PPID - %d \n", childPID, childPPID);
    } else {
        pid_t parentPID = getpid();
        pid_t parentPPID = getppid();
        pid_t parentChildPID = pid;
        printf("Parent PID - %d \n Parent PPID - %d \n Child PID form Parent - %d \n\n", parentPID, parentPPID, parentChildPID);
    }
    return 0;
}