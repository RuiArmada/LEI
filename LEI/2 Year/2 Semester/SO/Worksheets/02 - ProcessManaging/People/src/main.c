#include "../include/people.h"

int main(int argc , char const *argv[]) {
    if(argc < 4) {
        puts("ERROR! Wrong number of arguments...");
        return 1;
    }
    int n_output = 0;
    if(argc == 5) 
        if(strcmp(argv[4] , "--no-output") == 0) 
            n_output = 1;  
    int fd;
    PERSON person;
    switch(*(argv[1] + 1)) {

        case 'i':
            if(!n_output)
                puts("Insert Mode Enabled.");
            fd = open("Registry", O_WRONLY | O_CREAT | O_APPEND, S_IRUSR | S_IWUSR);
            if(fd == -1) {
                puts("ERROR - Couldn't WRITE to registry file...");
                close(fd);
                return 1;
            }
            struct stat st;
            fstat(fd , &st)
            off_t file_size = st.st_size;
            int pos = (int)file_size / sizeof(struct person);
            if(!n_output)
                printf("Registry Successfull - registy %d\n", pos);
            close(fd);
            break;
        
        case 'u':
            if(!n_output)
                puts("Update Mode Enabled.")
            int fd = open("Registry", O_RDWR);
            if(fd == -1) {
                puts("ERROR - Couldn't OPEN the registry file...");
                return 1;
            }
            int registry = -1;
            int i = 1;
            if(isdigit(*argv[2]))
                registry = atoi(argv[2]);
            while(reaf(fd , &person , sizeof(struct person)) > 0) {
                if((registry == -1 && strcmp(person.name , argv[2]) == 0) || registry == i) {
                    person.age = atoi(argv[3]);
                    lseek(fd , - sizeof(struct person) , SEEK_CUR);
                    if(write(fd , &person , sizeof(struct person)) < 1) {
                        puts("ERROR - Couldn't WRITE to registry file...");
                        close(fd);
                        return 1;
                    }
                    if(!n_output)
                        puts("UPDATE successful");
                    goto SUCCESS;
                }
                i++;
            }
            puts("ERROR - Registry not found");
            SUCCESS:
            close(fd);
            break;

        default:
            puts("ERROR - Invalid Key...");
            break;
    }
    if(!n_output) {
        fd = open("Registry" , O_RDONLY);
        while(read(fd , &person , sizeof(struct person)) > 0)
            printf("%s - %d\n", person.name , person.age);
        close(fd);
    }
    return 0;
}