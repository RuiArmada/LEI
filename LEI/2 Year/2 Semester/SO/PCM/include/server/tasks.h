#ifndef SO_TASKS_H
#define SO_TASKS_H

#include <stddef.h>
#include <stdbool.h>
#include <unistd.h> 

typedef struct {
    pid_t pid;
    long long taskID;
    char *task;
} Task;

typedef struct {
    Task *tasks;
    size_t size;
    size_t capacity;
} Tasks;

Tasks tasks_create();
void tasks_add(Tasks *this, Task task);
void tasks_list(Tasks const *this, int fd);
bool kill_task(Tasks *this, long long tid);
void remove_task(Tasks *this, pid_t pid);
void free_tasks(Tasks *this);

#endif //SO_TASKS_H
