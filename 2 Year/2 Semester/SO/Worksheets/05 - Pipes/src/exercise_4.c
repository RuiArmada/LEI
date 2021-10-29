#include "../include/include.h"

int main(int argc , char const *argv[]) {
    int pipes[2];
    if(pipe(pipes) < 0) {
        perror("pipe");
        exit(1);
    }
    if(fork() == 0) {
        close(pipes[0]);
        dup2(pipes[1] , STDOUT_FILENO);
        close(pipes[1]);
        execlp("ls" , "ls" , "/etc" , NULL);
        _exit(1);
    }
    close(pipes[1]);
    if(fork() == 0) {
        dup2(pipes[0] , STDIN_FILENO);
        close(pipes[0]);
        execlp("wc" , "wc" , "-l" , NULL);
        _exit(1);
    }
    close(pipes[0]);
    if(wait(NULL) == -1)
        puts("ERROR");
    if(wait(NULL) == -1)
        puts("ERROR");
    return 0;
}