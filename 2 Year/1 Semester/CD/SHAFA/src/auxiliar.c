#include "../includes/auxiliar.h"

bool check_extention(char* path, char* ext) {
    size_t diff;
    if(path) {
        diff = strlen(path) - strlen(ext);
        if(diff >= 0)
            return (!strcmp(path + diff, ext));
    }
    return false;
}

char* add_extention(char* path, char* ext) {
    size_t len_p = strlen(path);
    size_t len_e = strlen(ext);
    char* new = NULL;
    new = malloc(len_p + len_e + 1);
    if(new) {
        memcpy(new, path, len_p);
        memcpy(new + len_p, ext, len_e + 1);
    }
    return new;
    free(new);
}

char* rm_extention(char* path) {
    size_t len_p_no_e;
    char* path_e = strrchr(path, '.');
    char* new = NULL;
    len_p_no_e = path_e ? path_e - path: strlen(path);
    new = malloc(len_p_no_e + 1);
    if(new) {
        memcpy(new, path, len_p_no_e);
        new[len_p_no_e] = '\0';
    }
    return new;
    free(new);
}