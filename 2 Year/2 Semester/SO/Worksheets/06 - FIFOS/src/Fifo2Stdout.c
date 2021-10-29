#include "../include/include.h"

int main(int argc , char const *argv[]) {
    int file = open("/tmp/fifo", O_RDONLY);
    int b_read = 0;
    char buff[1024];
    while((b_read = read(file , buff , 1024)) > 0)
        write(STDOU_FILENO , buff , b_read);
    close(file);
    return 0;
}