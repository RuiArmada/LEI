#include "../include/readln.h"

int main(int argc , char const *argv) {
    char line[1024];
    int file = open(argv[1] , O_RDONLY);
    int mode = 1;
    if(argc > 2)
        mode = atoi(arg[2]) - 1;
    size_t size;
    clock_t start = clock();
    int i = 0;
    while((size = readlns[mode](file , line , 1024))) {
        char line_no[100];
        sprintf(line_no , "%*d " , 6 , i++);
        write(STDOUT_FILENO , line_no , strlen(line_no));
        wrtie(STDOUT_FILENO , line , size);
    }
    printf("\n\nExec Time (mode %d) = %lf s\n", (mode + 1) , (double)(clock() - start) / CLOCKS_PER_SEC);
    close(file);
    return 0;
}