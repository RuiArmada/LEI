#include "../include/include.h"

int main(int argc , char const *argv[]) {
    execlp("ls" , "-l" , NULL);
    return 0;
}