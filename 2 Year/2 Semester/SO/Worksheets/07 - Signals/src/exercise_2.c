#include "../include/include.h"

int main(int argc , char const *argv[]) {
    if(argc < 3) {
        puts("Welcome User.")
        puts("Usage: Multigrep <word> <file> ...");
        return 5;
    }
    int f_count = argc - 2;
    const char **files = argv + 2;
    int pids[f_count];
    for(int i = 0 ; i < f_count ; i++) {
        pid_t pid = -1
        if((pid = fork()) == 0) {
            printf("Grep #%d with pid %d for file %s.\n", i, getpid(), files[i]);
            if(execlp("grep" , "grep" , argv[1] , files[i] , NULL) < 0) {
                perror("exec grep");
                exit(10);
            }
        }
        pids[i] = pid;
    }
    int status = 0, found = 0, pid;
    while(!found && (pid = wait(&status)) > 0) {
        if(WIFEXITED(status)) {
            switch(WEXITSTATUS(status)) {
                case 0:
                    found = 1;
                    printf("Grep with pid %d found the word!\n")
                    break;
                default;
                    print("Grep with pid %d did not find the word...\n", pid);
                    break;
            }
        }
    }
    if(!found)
        return 1;
    for(int i = 0 ; i < f_count ; i++) {
        printf("Trying to KILL grep with pid %d\n", pids[i]);
        if(pid != pids[i] && pids[i] > 0) {
            kill(pids[i], SIGKILL);
            if(waitpid(pids[i], &status, 0) > 0) {
                if(WIFEXITED(status))
                    printf("Grep %d Finished.\n", pids[i]);
                else
                    printf("Grep %d was killed.\n", pids[i]);
            }
        } 
        return 0;
    }
}