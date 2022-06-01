#include "server/tasks.h"
#include "common/common.h"

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <stdbool.h> 

Tasks tasks_create() {
   return (Tasks) {.tasks = NULL, .size = 0, .capacity = 0};
}

void tasks_add(Tasks *this, Task task) {
    if (this->size == this->capacity) {
        this->capacity = this->size == 0 ? 1 : this->size * 2;
        this->tasks = realloc(this->tasks, this->capacity * sizeof(Task));
    }
    this->tasks[this->size++] = task;
}

void tasks_list(Tasks const *this, int fd) {
    for (size_t i = 0; i < this->size; ++i) {
        char *task = malloc(strlen("#: \n") + countNumberOfChars(this->tasks[i].taskID) + strlen(this->tasks[i].task) + 1);
        sprintf(task, "#%lld: %s\n", this->tasks[i].taskID, this->tasks[i].task);
        write(fd, task, strlen(task));
    }
}

static void removeByIndex(Tasks *this, size_t index) {
    free(this->tasks[index].task);
    this->tasks[index] = this->tasks[this->size-1];
    this->size--;
}

bool kill_task(Tasks *this, long long tid) {
    for (size_t i = 0; i < this->size; ++i) {
        if(this->tasks[i].taskID == tid) {
            kill(this->tasks[i].pid, SIGTERM);
            removeByIndex(this, i);
            return true;
        }
    }
    return false;
}

void remove_task(Tasks *this, pid_t pid) {
    for (size_t i = 0; i < this->size; ++i) {
        if(this->tasks[i].pid == pid) {
            removeByIndex(this, i);
        }
    }
}

void free_tasks(Tasks *this) {
    for (size_t i = 0; i < this->size; ++i) {
        free(this->tasks[i].task);
    }
}