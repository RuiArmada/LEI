#include "../include/include.h"

int main(int argc , char const* argv[]) {
    int pipes[2];
    if(pipe(pipes) < 0) {
        perror("pipe");
        exit(1);
    }
    int pid;
    if((pid = fork()) == 0) {
        close(pipes[1]);
        dup2(pipes[0] , STDIN_FILENO); 
        close(pipes[0]);
        execlp("wc" , "wc" , NULL);
    } else {
        close(pipes[0]);
        int bytes;
        char buff[10];
        while((bytes = read(STDIN_FILENO , buff , 10)) > 0)
            write(pipes[1] , buff , bytes);
        close(pipes[1]);
        wait(NULL);
    }
    return 0;
}