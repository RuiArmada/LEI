#include "../include/include.h"

int main(int argc , char const *argv[]) {
    int pipes[2];
    if(pipe(pipes) < 0) {
        perror("pipe");
        exit(1);
    }
    int pid = -1;
    if((pid = fork() == 0)) {
        close(pipes[1]);
        char buff[10];
        int bytes = read(pipes[0] , buff , 10);
        close(pipes[0]);
        write(STDOUT_FILENO , buff , bytes);
        _exit(0);
    } else {
        close(pipes[0]);
        char* str = "test";
        write(pipes[1] , str , strlen(str));
        close(pipes[1]);
        wait(NULL);
    }
    return 0;
}