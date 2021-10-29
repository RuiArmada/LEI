#include "../includes/moduleF.h"
#include "../includes/moduleT.h"
#include "../includes/moduleC.h"
#include "../includes/moduleD.h"
#include "../includes/error.h"
#include "../includes/file.h"
#include "../includes/auxiliar.h"
#include "../includes/threads.h"



typedef struct {
    unsigned long size;
    bool m_f, m_t, m_c, m_d, force_RLE, force_FREQ, d_SHAFA, d_RLE;
} OPT;



bool keys(int argc, char * argv[], OPT * option, char ** file) {
    char opt, *key, *val;
    for(int i = 1 ; i < argc ; i++) {
        key = argv[i];
        if(strcmp(key, "--no--thread") == 0)
            NO_THREAD = true;
        else if (key[0] != '-') {
            if(*file)
                return false;
            *file = key;
        } else{
            if(++i >= argc)
                return false;
            val = argv[i];
            if(strlen(key) != 2 || strlen(val) != 1)
                return false;
            opt = *val;
            switch(key[1]) {
                case 'm':
                    switch(opt) {
                        case 'f':
                            option->m_f = true;
                            break;
                        case 't':
                            option->m_t = true;
                            break;
                        case 'c':
                            option->m_c = true;
                            break;
                        case 'd':
                            option->m_d = true;
                            break;
                        default:
                            return 0;
                    }
                    break;
                case 'b':
                    switch(opt) {
                        case 'K':
                            option->size = _640KiB;
                            break;
                        case 'm':
                            option->size = _8MiB;
                            break;
                        case 'M':
                            option->size = _64MiB;
                            break;
                        default:
                            return 0;
                    }
                    break;
                case 'c':
                    if(opt == 'r')
                        option->force_RLE = true;
                    else if(opt == 'f')
                        option->force_FREQ = true;
                    else 
                        return false;
                    break;
                case 'd':
                    if(opt == 's')
                        option->d_SHAFA = true;
                    else if(opt == 'r')
                        option->d_RLE = true;
                    else    
                        return false;
                    break;
                default:
                    return false;
            }
        }
    }
    return true;
}



ERROR exec(OPT option, char** file) {
    ERROR error;
    char * temp;
    bool f_RLE_SHAFA = false, decomp = false;
    if(option.m_f) {
        error = freq_rle(file, option.force_RLE, option.force_FREQ, option.size);
        if(error) {
           fputs("ERROR -> Module F\n", stderr);
           return error; 
        }   
    }
    if(option.m_t) {
        if(!option.m_f) {
            if(check_extention(*file, FREQ)) {
                temp = rm_extention(*file);
                if(!temp)
                    return _NO_MEMORY;
                free(*file);
                *file = temp;
            } else {
                fprintf(stderr, "Module T: Wrong Extension. Should end like %s\n", FREQ);
                return _OUTSIDE;    
            }
        }
        error = get_CODES(*file);
        if(error) {
            fputs("ERROR -> Module T\n", stderr);
            return error;
        }
    }
    if(option.m_c) {
        if(option.m_f && !option.m_t) {
            fputs("Module C: Can't Execute ModuleC after ModuleF without ModuleT.\n", stderr);
            return _OUTSIDE;
        }
        error = shafa(file);
        if(error) {
            fputs("ERROR -> Module C\n", stderr);
            return error;
        }
    }
    if(option.m_d) {
        if((option.m_f && (!option.m_t || !option.m_c) && !check_extention(*file, RLE)) || (option.m_t && !option.m_c)) {
            fputs("Module D: Can't Execute ModuleD after ModuleF without ModuleT or ModuleC, nor Execute it after ModuleT without ModuleC.\n", stderr);
            return _OUTSIDE;
        }
        if(option.d_SHAFA || !option.d_RLE) {
            if(!check_extention(*file, SHAFA)) {
                if(option.d_SHAFA) {
                    fprintf(stderr, "Module D: Wrong Extension. Should end like %s\n", SHAFA);
                    return _OUTSIDE;
                }
            } else {
                if(option.d_RLE) {
                    if(check_extention(*file, RLE SHAFA))
                        f_RLE_SHAFA = true;
                    else {
                        fprintf(stderr, "Module D: Wrong Extension. Should end like %s\n", RLE SHAFA);
                        return _OUTSIDE;
                    }
                }
                error = decompress_SHAFA(file, (option.d_RLE || !option.d_SHAFA) && (f_RLE_SHAFA || check_extention(*file, RLE SHAFA)));
                if(error) {
                    fputs("ERROR -> Module D\n", stderr);
                    return error;
                } else
                    decomp = true;
            }
        }
        if(!decomp && (option.d_RLE || !option.d_SHAFA)) {
            if(!check_extention(*file, RLE)) {
                fprintf(stderr, "Module D: Wrong Extension. Should end like %s\n", RLE);
                return _OUTSIDE;
            }
            error = decompress_RLE(file);
            if(error) {
                fputs("ERROR -> Module D\n", stderr);
                return error;
            }
        }
    }
    return _SUCCESS;
}



int main(const int argc, char* const argv[]) {
    OPT option = {0};
    char * file = NULL;
    int error;
    if(argc <= 1) {
        fputs("No File Input\n", stderr);
        return 1;
    }
    if(!keys(argc, argv, &option, &file)) {
        fputs("Wrong Option\n", stderr);
        return 1;
    }
    if(!file) {
        fputs("No File Input\n", stderr);
        return 1;
    }
    file = add_extention(file, "");
    if(!file) {
        fputs("No Memory\n", stderr);
        return 1;
    }
    if(!option.m_f && !option.m_t && !option.m_c && !option.m_d) {
        if(check_extention(file, SHAFA))
            option.m_d = 1;
        else
            option.m_f = option.m_t = option.m_c = 1;
    }
    if(!option.size)
        option.size = _64KiB;
    error = exec(option, &file);
    free(file);
    if(error) {
        if(error != _OUTSIDE)
            fputs(hades_prtcl(error), stderr);
        return 1;
    }
    return 0;
}