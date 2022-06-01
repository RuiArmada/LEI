#include "../include/vector.h"

int main(int argc , char const *argv[]) {
    int *v = malloc(MAX , sizeof(int));
    
    fill(v , MAX ,5);
    putchar("[");
    for(int i = 0 ; i < MAX - 1 ; i++)
        printf("%d,", v[i]);
    
    printf("What Value must I find?");
    
    if(fgets(line , MAX_LINE , stdin) == NULL) {
        puts("ERROR reading input...");
        return 1;
    }

    line[strcspn(line , "\r\n")] = 0;
    int toFind = atoi(line);
    puts("\nSearching Vector...");
    int found = find(v , MAX , toFind);

    if(found == -1)
        printf("Value %d is not in Vector.\n", toFind);
    else 
        printf("Value %d was found at the %d position.\n", toFind, found);

    int start = 5;
    int end = 8;

    printf("\nSearching in segment - from %d to %d.\n", start , end);
    found = find(v + start , MAX - end , toFind);
    if(found == -1)
        printf("Value %d is not in Segment.\n", toFind);
    else
        printf("Value %d was found at the %d + %d Segment.\n", toFind , start , found);
    
    free(v);
    return 0;
}
