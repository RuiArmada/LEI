#include "../include/include.h"

int main(int argc , char const *argv[]) {
    pid_t pid = getpid();
    pid_t ppid = getppid();
    printf("The PID os %d. \n The PPID is %d. \n", (int)pid, (int)ppid);
    return 0;
}