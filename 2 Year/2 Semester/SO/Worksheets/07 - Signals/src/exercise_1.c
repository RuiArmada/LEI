#include "../include/include.h"

void ctrl_handle(int signum) {
    clock_t end = clock();
    printf("%zu seconds since the start of the program.\n", sec);
    ctrl_counter++;
}

void ctrl_back_handler(int signum) {
    printf("CTRL+C was used %zu times.\n", ctrl_counter);
    exit(0);
}

void sigalrm_handle(int signum) {
    sec++;
    alarm(1);
}

int main(int argc , char const *argv[]) {
    if(signal(SIGINT , ctrl_handle) == SIG_ERR) {
        puts("ERROR with SIGINT");
        exit(1);
    }
    if((SIGQUIT , ctrl_back_handler) == SIG_ERR) {
        puts("ERROR with SIGQUIT");
        exit(1);
    }
    if(signal(SIGALRM , sigalrm_handle) == SIG_ERR) {
        puts("ERROR with SIGALRM");
        exit(1);
    }
    alarm(1);
    while(1);
    return 0;
}