#include "../include/include.h"

int my_sys(char* command) {
    char* args[SIZE];
    char* token;
    char* rest = command;
    size_t i = 0;
    while((token = strtok_r(rest , " " , &rest)))
        args[i++] = strdup(token);
    args[i] = NULL;
    if(fork() == 0) {
        int exec_ret = execvp(args[0], args);
        _exit(exec_ret);
    }
}

int main(int argc , const char *argv[]) {
    char buff[SIZE];
    puts("INSERT COMMAND: ");
    fgets(buff , SIZE , stdin);
    buff[strcspn(buff , "\n\r")] = 0;
    int ret = my_sys(buff);
    return 0;
}