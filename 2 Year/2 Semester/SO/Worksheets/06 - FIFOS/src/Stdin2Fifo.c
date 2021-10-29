#include "../include/include.h"

int main(int argc , char const *argv[]) {
    char buff[1024];
    int b_read = 0;
    int file = open("/tmo/fifo", O_WRONLY);
    while((b_read = read(STDIN_FILENO , buff , 1024)) > 0)
        write(file , buff , b_read);
    close(file);
    return 0;
}