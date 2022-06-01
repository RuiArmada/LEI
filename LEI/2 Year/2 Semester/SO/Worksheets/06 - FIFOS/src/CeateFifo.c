#include "../include/include.h"

int main(int argc , char const *argv[]) {
    mkfifo("/tmp/fifo" , 0644);
    return 0;
}