#include "../include/include.h"

int main(int argc , char const *argv[]) {
    if(argc < 3) {
        puts("Wrong number of arguments...");
        return 1;
    }
    if(argc == 4)
        BUFF_SIZE = atoi(argv[3]);
    clock_t start = clock();
    int content = open(argv[1] , O_RDONLY);
    if(content == -1) {
        printf("ERROR - %s - No such file/directory\n", argv[1]);
        exit(1);
    }
    int dest = open(argv[2] , O_WRONLY | O_CREAT | O_TRUNC , 0644);
    char *buff = malloc(BUFF_SIZE);
    ssize_t b_read;
    while((b_read = read(content , buff , BUFF_SIZE)) > 0) 
        write(dest , buff , b_read);
    printf("Exec Time = %lfs\n", (double)(clock() - start) / CLOCKS_OER_SEC);
    close(content);
    close(dest);
    return 0;
}