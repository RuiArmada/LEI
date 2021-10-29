#include "../includes/moduleD.h"

ERROR load_RLE(FILE* f_rle, unsigned long block_size, uint8_t** buff) {
    ERROR error = _SUCCESS;
    
    *buff = malloc(block_size);
    if(*buff) {
        if(fread(*buff, sizeof(uint8_t), block_size, f_rle) != block_size) {
            error = _FILE_STREAM_FAILED;
            *buff = NULL;
        }
    }
    else
        error = _NO_MEMORY;
    return error;
}



ERROR decompress_RLE_block(void* _args) {
    ERROR error = _SUCCESS;
    ARGUMENTS_RLE* args = (ARGUMENTS_RLE*) _args;
    unsigned long block_size = args->rle_size, origin_size, l;
    unsigned long* final = args->final_size;
    uint8_t* buff = args->buffer;
    uint8_t* sequence;
    uint8_t repetitions;
    char simbol;

    if(block_size <= _64KiB)
        origin_size = _64KiB + _1KiB;
    else if(block_size <= _640KiB)
        origin_size = _640KiB + _1KiB;
    else if(block_size <= _8MiB)
        origin_size = _8MiB + _1KiB;
    else
        origin_size = _64MiB + _1KiB;

    sequence = malloc(origin_size);
    if(sequence) {
        l = 0;
        for(unsigned long i = 0 ; i< block_size ; ++i) {
            simbol = buff[i];
            repetitions = 0;
            if(!simbol) {
                simbol = buff[++i];
                repetitions = buff[++i];
            }
            if(l + repetitions > origin_size) {
                switch(origin_size) {
                    
                    case _64KiB + _1KiB:
                        origin_size = _640KiB + _1KiB;
                        break;
                    
                    case _640KiB + _1KiB:
                        origin_size = _8MiB + _1KiB;
                        break;

                    case _8MiB + _1KiB:
                        origin_size = _64MiB + _1KiB;
                        break;

                    default:
                        error = _FILE_UNRECOGNIZABLE;
                        free(sequence);
                        return error;
                        
                }
                sequence = realloc(sequence, origin_size);
                if(!sequence) {
                    error = _NO_MEMORY;
                    free(sequence);
                    break;
                }
            }
            if(repetitions) {
                memset(sequence + l, simbol, repetitions);
                l += repetitions;
            } else
                sequence[l++] = simbol;
        }
        *final = l;
        args->seq = sequence;
    }else
        error = _NO_MEMORY;
    
    free(buff);
    return error;
}



ERROR write_decomp_RLE(void* _args, ERROR previous, ERROR error) {
    ARGUMENTS_RLE* args = (ARGUMENTS_RLE*) _args;
    FILE* f_write = args->f_write;
    unsigned long new_b_size = *args->final_size;
    uint8_t* sequence = args->seq;

    if(!error) {
        if(!previous) {
            if(fwrite(sequence, sizeof(uint8_t), new_b_size, f_write) != new_b_size)
                error = _FILE_STREAM_FAILED;
        }
        free(sequence);
    }
    free(_args);
    return error;
}



ERROR decompress_RLE (char ** path) {
    ERROR error = _SUCCESS;
    FILE *f_rle, *f_freq, *f_wrt;
    char *path_freq, *path_wrt, *path_rle;
    uint8_t * buff;
    char mode;
    unsigned long *rle_sizes, *final_sizes;
    unsigned long long length;
    float total_time;
    ARGUMENTS_RLE * args;
    
    clock_thread(START);
    path_rle = *path;
    f_rle = fopen(path_rle, "rb");
    if (f_rle) {
        path_wrt = rm_extention(path_rle);
        if (path_wrt) {
            f_wrt = fopen(path_wrt, "wb");
            if (f_wrt) {
                path_freq = add_extention(path_rle, FREQ);
                if (path_freq) {
                    f_freq = fopen(path_freq, "rb");
                    if (f_freq) {
                        if (fscanf(f_freq, "@%c@%lu", &mode, &length) == 2) {   
                            if (mode == 'R' || mode == 'r') {
                                rle_sizes = malloc(sizeof(unsigned long) * length);       
                                if (rle_sizes) {
                                    for (unsigned long long i = 0; i < length && !error; ++i) {
                                        if (fscanf(f_freq, "@%lu@%*[^@]", rle_sizes + i) != 1)                                          
                                            error = _FILE_STREAM_FAILED;                                                                              
                                    }
                                    if (error) 
                                        free(rle_sizes);
                                }   
                                else 
                                    error = _NO_MEMORY;                      
                            }
                            else 
                                error = _FILE_UNRECOGNIZABLE;
                        }
                        else
                            error = _FILE_STREAM_FAILED;  
                            
                        fclose(f_freq);                   
                    }
                    else 
                        error = _FILE_INACCESSIBLE;

                    free(path_freq);
                }   
                else 
                    error = _NO_MEMORY;  

                if (!error) {
                    final_sizes = malloc(sizeof(unsigned long) * length);
                    if (final_sizes) {
                        for (unsigned long long thread_idx = 0; thread_idx < length; ++thread_idx) {                               
                            error = load_RLE(f_rle, rle_sizes[thread_idx], &buff);
                            if (error) break;
                            args = malloc(sizeof(ARGUMENTS_RLE)); 
                            if (!args) {
                                free(buff);
                                break;
                            }
                            *args = (ARGUMENTS_RLE) {

                                .rle_size = rle_sizes[thread_idx],
                                .buffer = buff,
                                .f_rle = f_rle, 
                                .f_write = f_wrt,
                                .final_size = &final_sizes[thread_idx]

                            };       

                            error = thread_create(decompress_RLE_block, write_decomp_RLE, args);                                
                            if (error) {
                                free(args);
                                free(buff);
                                break;
                            }    
                        }
                        multithread_wait();                                        
                        if (error) 
                            free(final_sizes);                  
                    }
                    else 
                        error = _NO_MEMORY;
                }
                fclose(f_wrt);                    
            }
            else 
                error = _FILE_INACCESSIBLE;
        }
        else 
            error = _NO_MEMORY;
        
        fclose(f_rle);
    }
    else 
        error = _FILE_INACCESSIBLE;

    if (!error) {        
        free(path_rle);
        *path = path_wrt;
        total_time = clock_main_thread(STOP);
        print_summary(total_time, rle_sizes, final_sizes, length, *path, _RLE);
        free(rle_sizes);
        free(final_sizes);
    }

    return error;
}



void release_tree(BTREE tree) {
    if(tree) {
        release_tree(tree->right);
        release_tree(tree->left);
        free(tree);
    }
}



ERROR add_Tree(BTREE* decode, char* code, int start, int end, char symbol) {
    for(int i = start ; i < end ; ++i) {
        if(*decode && code[i] == '0')
            decode = &(*decode)->left;
        else if(*decode && code[i] == '1')
                decode = &(*decode)->right;
        else {
            *decode = malloc(sizeof(struct btree));
            if(!(*decode))
                return _NO_MEMORY;
            (*decode)->left = (*decode)->right = NULL;
            if(code[i] == '0')
                decode = &(*decode)->left;
            else
                decode = &(*decode)->right;
        }
    }
    *decode = malloc(sizeof(struct btree));
    (*decode)->symbol = symbol;
    (*decode)->left = (*decode)->right = NULL;
    return _SUCCESS;
}



ERROR creation_tree(char* code, BTREE* decode) {
    ERROR error = _SUCCESS;
    int j, start, end;

    *decode = malloc(sizeof(struct btree));
    if(*decode) {
        (*decode)->right = (*decode)->right = NULL;
        for(int symbol = 0, l = 0 ; code[l] && !error ; ) {
           while(code[l] == ';') {
               symbol++;
               l++;
           }
           start = l;
           for( ; code[l] && (code[l] != ';') ; ++l);
           end = l;
            if(start != end)
                error = add_Tree(decode, code, start, end, symbol);
        }
        free(code);
    }
    else
        error = _NO_MEMORY;
    return error;
}



ERROR decompress_SHAFA_block(uint8_t* shafa, unsigned long size, BTREE decode, uint8_t** decomp) {
    BTREE root;
    uint8_t mask;
    unsigned long i, l;
    int bit;

    *decomp = malloc(size);
    if(!(*decomp))
        return _NO_MEMORY;
    root = decode;
    mask = 128;
    i = l = 0;
    while(l < size) {
        bit = mask & shafa[i];
        if(!bit)
            decode = decode->left;
        else
            decode = decode->right;
        if(decode && !(decode->left) && !(decode->right)) {
            (*decomp)[l++] = decode->symbol;
            decode = root;
        }
        mask >>= 1;
        if(!mask) {
            ++i;
            mask = 128;
        }
    }
    return _SUCCESS;
}



ERROR process_SHAFA(void* _args) {
    ERROR error;
    ARGUMENTS_SHAFA* args_shafa = (ARGUMENTS_SHAFA*) _args;
    BTREE decode;
    ARGUMENTS_RLE args_rle;

    error = creation_tree(args_shafa->code_ID, &decode);
    if(!error) {
        error = decompress_SHAFA_block(args_shafa->shafa_code, *args_shafa->rle_size, decode, &args_shafa->shafa_decomp);
        free(args_shafa->shafa_code);
        free_tree(decode);
        if(!error && args_shafa->rle_decompress) {
            args_rle = (ARGUMENTS_RLE) {
                .buffer = args_shafa->shafa_decomp,
                .rle_size = *args_shafa->rle_size,
                .final_size = args_shafa->final_size
            };
            error = decompress_RLE_block(&args_rle);
            if(!error)
                args_shafa->rle_decomp = args_rle.seq;
        }
    }
    return error;
}



ERROR write_decomp_SHAFA(void* _args, ERROR previous, ERROR error) {
    ARGUMENTS_SHAFA* args_shafa = (ARGUMENTS_SHAFA*) _args;
    unsigned long size_write;
    uint8_t* decomp;
    FILE* f_write = args_shafa->f_write;
    bool rle_decompress = args_shafa->rle_decompress;

    if(!error) {
        if(!previous) {
            size_write = (rle_decompress) ? (*args_shafa->final_size) : (*args_shafa->rle_size);
            decomp = (rle_decompress) ? (args_shafa->rle_decompress) : (args_shafa->shafa_decomp);
            if(fwrite(decomp, sizeof(uint8_t), size_write, f_write) != size_write)
                error = _FILE_STREAM_FAILED;
        }
        if(rle_decompress)
            free(args_shafa->rle_decomp);
    }
    free(_args);
    return error; 
}



ERROR decompress_SHAFA(char** path, bool decompress_rle) {
    ERROR error;
    FILE *f_SHAFA, *f_COD, *f_WRT;
    char *path_COD, *path_WRT, *path_SHAFA, *path_TMP, mode;
    uint8_t* shafa_code;
    char* code_ID;
    float total_time;
    unsigned long long length;
    unsigned long *size, *sizes, *final_sizes, bsize;
    ARGUMENTS_SHAFA* args;

    size = sizes = final_sizes = NULL;
    path_SHAFA = *path;
    error = _SUCCESS;
    clock_thread(START);
    f_SHAFA = fopen(path_SHAFA, "rb");
    if (f_SHAFA) {
        path_TMP = rm_ext(path_SHAFA);
        if (path_TMP) {
            if (decompress_rle) {
                path_WRT = rm_ext(path_TMP);
                if (!path_WRT) 
                    error = _NO_MEMORY;
            }
            else 
                path_WRT = path_TMP;
            f_WRT = fopen(path_WRT, "wb");
            if (f_WRT) {                
                path_COD = add_ext(path_TMP, CODES);
                if (path_COD) {
                    f_COD = fopen(path_COD, "rb");
                    if (f_COD) {
                        if (fscanf(f_SHAFA, "@%lu", &length) == 1) {
                            if (fscanf(f_COD, "@%c@%lu", &mode, &length) == 2) {
                                if ((mode == 'N' && !decompress_rle) || (mode == 'R')) {   
                                    sizes = malloc(sizeof(unsigned long) * length);
                                    if (sizes) {
                                        sizes = malloc(sizeof(unsigned long) * length);
                                        if (size) {
                                            if (decompress_rle) {
                                                final_sizes = malloc(sizeof(unsigned long) * length);
                                                if (!final_sizes)
                                                    error = _NO_MEMORY;
                                            }  
                                            for (unsigned long long thread_idx = 0; thread_idx < length && !error; ++thread_idx) {
                                                if (fscanf(f_SHAFA, "@%lu@", &bsize) == 1) {
                                                    sizes[thread_idx] = bsize;                                                        
                                                    shafa_code = malloc(bsize); 
                                                    if (shafa_code) {
                                                        if (fread(shafa_code, sizeof(uint8_t), bsize, f_SHAFA) == bsize) { 
                                                            if (fscanf(f_COD, "@%lu", &size[thread_idx]) == 1) {
                                                                code_ID = malloc(33152);
                                                                if (code_ID) {
                                                                    if (fscanf(f_COD,"@%33151[^@]", code_ID) == 1) {
                                                                        args = malloc(sizeof(ARGUMENTS_SHAFA)); 
                                                                        if (!args) {
                                                                            error = _NO_MEMORY;
                                                                            free(shafa_code);
                                                                            break;
                                                                        }
                                                                        *args = (ARGUMENTS_SHAFA) {
                                                                            .final_size = f_WRT,
                                                                            .shafa_code = shafa_code,
                                                                            .rle_decompress = decompress_rle,
                                                                            .rle_size = &size[thread_idx],
                                                                            .final_size = &final_sizes[thread_idx],
                                                                            .code_ID = code_ID
                                                                        };
                                                                        error = thread_create(process_SHAFA, write_decomp_SHAFA, args); 
                                                                            
                                                                        if (error) {
                                                                            free(code_ID);
                                                                            break;
                                                                        }
                                                                    }
                                                                    else 
                                                                        error = _FILE_STREAM_FAILED;                                                    
                                                                }
                                                                else 
                                                                    error = _NO_MEMORY;
                                                            }
                                                            else 
                                                                error = _FILE_STREAM_FAILED;                                               
                                                        }
                                                        else 
                                                            error = _FILE_STREAM_FAILED;                             
                                                    }
                                                    else 
                                                        error = _NO_MEMORY;                                                      
                                                }
                                                else 
                                                    error = _FILE_STREAM_FAILED;

                                                } 
                                                thread_wait();
                                        }
                                        else 
                                            error = _NO_MEMORY;
                                    }
                                    else 
                                        error = _NO_MEMORY;                               
                                }
                                else 
                                    error = _FILE_UNRECOGNIZABLE;                           
                            }
                            else 
                                error = _FILE_STREAM_FAILED;
                        }
                        else 
                            error = _FILE_STREAM_FAILED;

                        fclose(f_COD);   
                    }
                    else 
                        error = _FILE_INACCESSIBLE;
                    
                    free(path_COD);
                }
                else 
                    error = _NO_MEMORY;

                fclose(f_WRT);
            }
            else 
                error = _FILE_INACCESSIBLE;
            
            if (decompress_rle) 
                free(path_TMP);
        }
        else 
            error = _NO_MEMORY;

        fclose(f_SHAFA);
    }
    else 
        error = _FILE_INACCESSIBLE;

    if (!error) {
        total_time = clock_thread(STOP);                                
        *path = path_WRT;
        free(path_SHAFA);
        if (decompress_rle) {
            raphael(total_time, sizes, final_sizes, length, path_WRT, _SHAFA_RLE); 
            free(final_sizes);
        }
        else 
            raphael(total_time, sizes, size, length, path_WRT, _SHAFA);                                               
    }                                
    if (size) 
        free(size);

    if (sizes) 
        free(sizes);
    
    return error;
}



void raphael(
                            double time,
                            unsigned long *decomp_size,
                            unsigned long *new_size,
                            unsigned long long length,
                            char* new_path,
                            ALGORITHM algo
                          ) {
    printf("+------------------- Raphael -------------------+\n");
    if(algo == _RLE)
        printf("|            Module: D (RLE Decoding)           |\n");
    else if(algo == _SHAFA)
        printf("|           Module: D (SHAFA Decoding)          |\n");
    else
        printf("|       Module: D (SHAFA & RLE Decoding)        |\n");

    printf("+-----------------------------------------------+");

    for(unsigned long long i = 0 ; i < length ; i++)
        printf("->Size before/after generating file (block %lu):  %lu%lu\n", i + 1, decomp_size[i], new_size[i]);
    printf("-> Module Runtime (in milliseconds): %f\n"
           "-> Generated File %s\n", time, new_path);
}