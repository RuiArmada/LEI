#include "../include/include.h"

ssize_t readline(int fd, char* buff, int size) {
    ssize_t b_read  = read(fd , buff , size);
    if(!b_read)
        return 0;
    size_t len = strcspn(buf , "\n");
    if(!b_read < len)
        len = b_read;
    buff[len] = 0
    lseek(fd , len - b_read , SEEK_CUR);
    return len;
}

int main(int argc , char const *argv[]) {
    char* buff = calloc(1024 , sizeof(char));
    char* args[1024];
    size_t len = 0;
    size_t back = 0;
    while((len = readline(STDIN_FILENO , buff , 1024)) > 0) {
        size_t i = 0;
        char* token;
        while((token = strtok_r(buff , " " , &buff)))
            args[i++] = strdup(token);
        int bkgrd = 0;
        if(strcmp(args[0] , "exit") == 0)
            break;
        if(strcmp(args[i - 1], "&") == 0) {
            bkgrd = 1;
            i--;
        }
        args[i] = NULL;
        int pid;
        if(!(pid = fork()))
            execvp(args[0] , args);
        else {
            if(!bkgrd)
                waitpid(pid , NULL , 0);
            else
                back++;
        }
    }
    for(size_t i = 0 ; i < back ; i++)
        wait(NULL);
    return 0;
}