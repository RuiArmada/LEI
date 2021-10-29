#include "../include/include.h"

int main(void) {
    char buff[1024];
    char* args[1024];
    args[0] = "./ex3"
    size_t i = 1;
    while(fgets(buff , 1024 , stdin)) {
        buff[strcspn(buff , "\n\r")] = 0;
        args[i++] = strdup(buff);
    } 
    args[i] = NULL;
    execv("./ex3", args);
    return 0;
}