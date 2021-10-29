#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdio.h>
#include <sys/select.h> 
#include <signal.h>
#include <stdbool.h>
#include <server/tasks.h>

#include "common/protocol.h"
#include "common/common.h"

#define READWRITE 0666
#define BUFSIZE 4096
#define POSLEITURA 0
#define POSESCRITA 1

long long nextTID = 0;

void strip_extra_spaces(char* str) {
    int i, x;
    for (i=x=0; str[i]; ++i) {
        if (str[i] == '\n') {
            break;
        } else if (!isspace(str[i]) || (i > 0 && !isspace(str[i - 1]) && str[i - 1] != '|' && str[i + 1] != '|')) {
            str[x++] = str[i];
        }
    }
    str[x] = '\0';
}

int countWords(const char *command) {
    int counter = 1;
    for (int i = 0; command[i]; ++i) {
        if(command[i] == ' ') {
            counter++;
        }
    }
    return counter;
}

void setStatus(long long tid, char *status) {
    int numCharstID = countNumberOfChars(tid);
    char *name = malloc(sizeof(char) * (strlen("/tmp/_status") + numCharstID + 1));
    sprintf(name, "/tmp/%lld_status", tid);
    int fd = open(name, O_CREAT | O_WRONLY | O_TRUNC, READWRITE);
    write(fd, status, strlen(status));
}

pid_t executeCommand(char* command, int input, int output) {
    int words = countWords(command);
    char *args[words + 1];
    char* line = strtok(command, " ");
    for (int i = 0; i < words; ++i) {
        args[i] = line;
        line = strtok(NULL, " ");
    }
    args[words] = NULL;
    pid_t pid = fork();
    if (pid == 0) {
        if (input != -1) {
            dup2(input, 0);
            close(input);
        }
        dup2(output, 1);
        close(output);
        execvp(args[0], args);
        _exit(1);
    }
    return pid;
}

int countPipes(const char* buffer) {
    int counter = 0;

    for (int i = 0; buffer[i]; i++) {
        if (buffer[i] == '|') {
            counter++;
        }
    }

    return counter;
}

int createTaskFile(long long tID, int fdOut, char* buf) {
    int numCharstID = countNumberOfChars(tID);
    char *name = malloc(sizeof(char) * (strlen("/tmp/") + numCharstID + 1));
    sprintf(name, "/tmp/%lld", tID);
    int fdO = open(name, O_CREAT | O_WRONLY | O_TRUNC, READWRITE);
    free(name);

    char *tid = malloc(sizeof(char) * (strlen("new task #\n") + numCharstID + 1));
    sprintf(tid, "new task #%lld\n", tID);


    write(fdOut, tid, strlen(tid) + 1);
    free(tid);

    write(fdO, "-> ", 3);
    write(fdO, buf, strlen(buf));
    write(fdO, "\n", 1);

    return fdO;
}

void finishExecution(int *p, int fdO) {
    int n;
    char results[BUFSIZE];

    close(p[POSESCRITA]);

    while ((n = read(p[POSLEITURA], results, BUFSIZE)) > 0) {
        write(fdO, results, n);
    }
    close(fdO);
    close(p[POSLEITURA]);
}

pid_t middleMan(int outP1, int inP2, long time, long long tid) {
    pid_t pid = fork();
    if(pid == 0) {
        char buf[BUFSIZE];

        while(1) {
            if (time > 0) { 
                struct timeval timeout;
                timeout.tv_sec = time;
                timeout.tv_usec = 0;
                fd_set set; 
                FD_SET(outP1, &set); 
                int res = select(outP1+1, &set, NULL, NULL, &timeout); 
                                                                       
                if (res == 0) {                                      
                    fprintf(stderr, "\n-> Task %lld timed out (inactivity time exceeded)\n", tid);
                    setStatus(tid, "Max inactivity");
                    kill(0, SIGTERM);
                }
            }
            int lido = read(outP1, buf, BUFSIZE);
            if(lido > 0) {
                write(inP2, buf, lido);
            } else {
                break;
            }
        }
        close(outP1);
        close(inP2);
        _exit(0);
    }
    return pid;
}

long long TID;

void timeout_handler(int signo) {
    if (signo != SIGALRM) {
        return;
    }
    fprintf(stderr, "\n-> Task %lld timed out (execution time exceeded)\n", TID);
    setStatus(TID, "Max execution");
    kill(0, SIGTERM);
}

pid_t executeCommands(char *buf, int fdOut, long time_inactive, long time_exec, long long tID) {
    pid_t exec_pid = fork();
    if(exec_pid != 0) {
       return exec_pid;
    }
    setsid(); 

    int numPipes = countPipes(buf) * 2;
    int pipes[numPipes + 1][2];
    char *strtoks[numPipes + 1];

    int fdO = createTaskFile(tID, fdOut, buf);

    for (int l = 0; l <= numPipes; ++l) {
        pipes[l][0] = 0;
        pipes[l][1] = 0;
    }

    strtoks[0] = strtok(buf, "|");
    for (int i = 1; i <= numPipes; i++) {
        if(i%2 == 0) {
            strtoks[i] = strtok(NULL, "|");
        } else {
            strtoks[i] = NULL;
        }
    }
    int input = -1;
    for (int j = 0; j <= numPipes; ++j) {
        pipe(pipes[j]);
        if(j%2 == 0) {
            executeCommand(strtoks[j], input, pipes[j][POSESCRITA]);
        } else {
            middleMan(input, pipes[j][POSESCRITA], time_inactive, tID);
        }
        close(pipes[j][POSESCRITA]);
        if (input != -1) close(input);

        input = pipes[j][POSLEITURA];
    }
    if(fork() == 0) {
        finishExecution(pipes[numPipes], fdO);
        _exit(0);
    }
    bool failed = false;
    TID = tID;
    if (time_exec != -1) {
        signal(SIGALRM, timeout_handler);
        alarm(time_exec);
    }
    while (1) {
        int status;
        pid_t res = wait(&status);
        if(res == -1) {
            break;
        }
        if(!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
            failed = true;
        }
    }
    fprintf(stderr, "\n-> Task %lld executed %s\n", tID, failed ? "unsuccessfully" : "successfully");
    setStatus(tID,failed? "Crashed" : "Finished");
    _exit(failed ? EXIT_FAILURE : EXIT_SUCCESS);
}

void sendOutput(char *buf, int fdOut) {
    char *filename = malloc(sizeof(char) * (strlen("/tmp/") + strlen(buf) + 1));
    sprintf(filename, "/tmp/%s", buf);

    int file = open(filename, O_RDONLY);
    char buffer[BUFSIZE];
    int lido;

    while ((lido = read(file, buffer, BUFSIZE)) > 0) {
        write(fdOut, buffer, lido);
    }
    END_OF_MESSAGE(fdOut);
    free(filename);
    close(file);
}

void printHistory(int fdOut) {
    for (int i = 0; i < nextTID; ++i) {
        char *filename = malloc(sizeof(char) * (strlen("/tmp/") + countNumberOfChars(i) + 1));
        sprintf(filename, "/tmp/%d", i);
        char *reason_file = malloc(sizeof(char) * (strlen("/tmp/_status") + countNumberOfChars(i) + 1));
        sprintf(reason_file, "/tmp/%d_status", i);

        int file = open(filename, O_RDONLY);
        int reason_file_d = open(reason_file, O_RDONLY);

        if(file < 0 || reason_file_d < 0) {
            goto CONTINUE;
        }
        char buf[BUFSIZE];
        int lido = readln(file, buf, BUFSIZE);
        char reason[BUFSIZE];
        int reason_len = readln(reason_file_d, reason, BUFSIZE);
        reason[reason_len] = '\0';
        char *printLine = malloc(sizeof(char) * (lido + countNumberOfChars(i) + reason_len + strlen("#, : ") + 1));
        sprintf(printLine, "#%d, %s: %s", i, reason, buf);
        write(fdOut, printLine, strlen(printLine));

        free(printLine);
        close(reason_file_d);
        close(file);
CONTINUE:
        free(reason_file);
        free(filename);
    }
    END_OF_MESSAGE(fdOut);
}

bool startsWith(char *buf, char *string, size_t size) {
    size_t len = strlen(string);
    if(len > size) {
        return false;
    }
    return strncmp(buf, string, len) == 0;
}

void handleClient(char *clientPipes) {
    WRITE_LITERAL(1, "\n\nA client connected\n");

    Tasks tasks = tasks_create();
    long time_inactive = -1;
    long time_exec = -1;
    char *out = clientPipes;
    char *in = strchr(clientPipes, ' ');
    *in = '\0';
    in += 1;

    int fdIn = open(in, O_RDWR);
    int fdOut = open(out, O_RDWR);

    WRITE_LITERAL(fdOut, SERVER_ACK);
    WRITE_LITERAL(1, "Ready for input\n");

    char buf[BUFSIZE];

    int lido;
    while ((lido = read(fdIn, buf, BUFSIZE)) > 0) {
        strip_extra_spaces(buf);
        if (strcmp(buf, "exit") == 0) {
            WRITE_LITERAL(fdOut, CLOSE);
            WRITE_LITERAL(1, "\nClient exited.\n\n");
            WRITE_LITERAL(1, "-------->>> READY FOR A NEW CLIENT <<<--------");
            close(fdIn);
            close(fdOut);
            free_tasks(&tasks);
            break;
        } else if (startsWith(buf, "output ", lido)) {
            sendOutput(buf + 7, fdOut);
        } else if (strcmp(buf, "history") == 0) {
            printHistory(fdOut);
        } else if (startsWith(buf, "inactive-time ", lido)) {
            long seconds = -1;
            sscanf(buf, "inactive-time %ld\n", &seconds);
            if (seconds <= 0) {
                WRITE_LITERAL(fdOut, "Not a valid number, try again.\n");
            } else {
                WRITE_LITERAL(fdOut, "Got it, thanks!\n");
                time_inactive = seconds;
            }
        } else if (startsWith(buf, "exec-time ", lido)) {
            long seconds = -1;
            sscanf(buf, "exec-time %ld\n", &seconds);
            if (seconds <= 0) {
                WRITE_LITERAL(fdOut, "Not a valid number, try again.\n");
            } else {
                WRITE_LITERAL(fdOut, "Got it, thanks!\n");
                time_exec = seconds;
            }
        } else if (strcmp(buf, "list") == 0) {
                tasks_list(&tasks, fdOut);
                END_OF_MESSAGE(fdOut);
        } else if (startsWith(buf, "terminate", lido)) {
            long long tid = -1;
            sscanf(buf, "terminate %lld\n", &tid);
            if (tid == 0) {
                WRITE_LITERAL(fdOut, "Not a valid number, try again.\n");
            } else {
                if (!kill_task(&tasks, tid)) {
                    WRITE_LITERAL(fdOut, "Task doesn't exist.\n");
                } else {
                    WRITE_LITERAL(fdOut, "Task terminated successfully\n");
                }
            }
        } else if (startsWith(buf, "execute ", lido)) {
            char *t = strdup(buf + strlen("execute "));
            pid_t  pid = executeCommands(buf + strlen("execute "), fdOut, time_inactive, time_exec, nextTID);
            Task task = (Task) {.pid = pid, .taskID = nextTID, .task = t};
            nextTID++;
            tasks_add(&tasks, task);
        } else {
            char *t = strdup(buf);
            pid_t  pid = executeCommands(buf, fdOut, time_inactive, time_exec, nextTID);
            Task task = (Task) {.pid = pid, .taskID = nextTID, .task = t};
            nextTID++;
            tasks_add(&tasks, task);
        }
        int status;
        pid_t pid;
        while((pid = waitpid(-1, &status, WNOHANG)) > 0) {
            remove_task(&tasks, pid);
        }
    }
}

int main() {
    WRITE_LITERAL(1, "Starting server\n");
    char *serverPipeName = "/tmp/server";
    mkfifo(serverPipeName, READWRITE);

    WRITE_LITERAL(1, "Opening serverPipe\n");
    char clientPipes[CLIENTPIPES_LEN];
    int serverPipe = open(serverPipeName, O_RDWR);

    WRITE_LITERAL(1, "Reading from serverPipe\n");
    while (read(serverPipe, clientPipes, CLIENTPIPES_LEN) == CLIENTPIPES_LEN) {
        handleClient(clientPipes);
    }
    close(serverPipe);
}
