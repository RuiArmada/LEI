#include "../includes/moduleF.h"

unsigned long block_compress(uint8_t buff[], uint8_t block[], unsigned long size, unsigned long size_f) {
    unsigned long i, j, size_rle;
    
    for(i = 0, size_rle = 0 ; i < size && i < size_f ; i = j) {
    
        int repetitions = 0;
    
        for(j = i ; buff[i] == buff[j] && repetitions < 255 && j < size ; ++j, ++repetitions);
    
        if(repetitions >= 4 || !buff[i]) {
            block[size_rle] = 0;
            block[size_rle + 1] = buff[i];
            block[size_rle + 2] = repetitions;
            size_rle += 3;
        } else {
            block[size_rle] = buff[i];
            size_rle++;
            j = ++i;
        }
    }

    return size_rle;
}



void make_freq(unsigned char* block, unsigned long* frequence, unsigned long size) {
    
    for(int i = 0 ; i < 256 ; i++)
        frequence[i] = 0;
    
    for(unsigned long j = 0 ; j < size ; j++) {
        int symbol;
        symbol = block[j];
        ++frequence[symbol];
    }
}



ERROR write_freq(unsigned long *freq, FILE* f_freq, unsigned long long block_ID, unsigned long long n_blocks) {
    int j, aux1 = 0, aux2 = 0, aux3 = 0;
    ERROR error = _SUCCESS;

    for(int i = 0 ; i < 256 ;) {
        aux1 = fprintf(f_freq, "%lu", freq[i]);
        if(aux1 >= 1) {
            for(j = i ; freq[i] == freq[j] && j < 256 ; j++) {
                if(j != 255) {
                    aux2 = fprintf(f_freq, ";");
                    if(aux2 < 0)
                        error = _FILE_STREAM_FAILED;
                }
            }
            i = j;
        }
        else
            error = _FILE_STREAM_FAILED;
    }
    if(block_ID == n_blocks - 1) {
        aux3 = fprintf(f_freq, "@0");
        if(aux3 < 1)
            error = _FILE_STREAM_FAILED;
    }
    
    return error;
}



void raphael(
                            unsigned long long n_blocks, 
                            unsigned long *size, 
                            unsigned long size_f, 
                            unsigned *size_rle,
                            double time, 
                            char* path_RLE, 
                            char* path_FREQ, 
                            char* path_RLE_FREQ
                          ) {
    
    printf("+------------------- Raphael -------------------+\n"
           "|        Module: F (Symbol Freq and Calc)       |\n"
           "+-----------------------------------------------+"
           "-> Number of blocks: %lu                         \n", n_blocks);

    printf("-> Size of Blocks in Origin File: ");
    for(unsigned long long i = 0 ; i < n_blocks ; i++) {
        if(i == n_blocks - 1)
            printf("%lu\n", size[i]);
        else
            printf("%lu/", size[i]);
    }
    if(path_RLE) {
        unsigned long size_RLE = 0;
        long compress;
        float ratio;
        for(unsigned long long j = 0 ; j < n_blocks ; j++) 
            size_RLE += size_rle[j];
        compress = size_f - size_RLE;
        ratio = (float)compress / (float)size_f;
        ratio *= 100.0;
        printf("-> RLE Compress: %s (%f%% compression)\n", path_RLE, ratio);
        printf("-> SIze of Blocks in RLE File: ");
        for(unsigned long long i = 0 ; i < n_blocks ; i++) {
            if(i == n_blocks - 1)
                printf("%lu bytes\n", size_rle[i]);
            else
                printf("%lu/", size_rle[i]);
        }
    }
    printf("-> Module Runtime (milliseconds): %f\n", time);
    printf("-> Generated Files: \n");
    if(path_RLE_FREQ && path_FREQ)
        printf("\t %s, %s\n", path_FREQ, path_RLE_FREQ);
    else if(path_FREQ)
        printf("\t %s\n", path_FREQ);
    else if(path_RLE_FREQ)
        printf("\t %s\n", path_RLE_FREQ);
}



ERROR freq_rle(char** path, bool doRLE, bool doFREQ, unsigned long size) {
    clock_t t;
    float time, ratio;
    uint8_t *buff, *block;
    int print_rle, print;
    long compress, size_last;
    unsigned long long n_blocks, block_ID;
    bool compress_rle;
    char *path_RLE = NULL, *path_RLE_FREQ = NULL, *path_FREQ = NULL;
    unsigned long size_f, block_size, size_rle, compressd, *block_sizes, *block_rle, *sizes, s;
    FILE *f, *f_RLE = NULL, *f_RLE_FREQ = NULL, *f_FREQ = NULL;

    compress_rle = true;
    size_last = 0;
    block_size = size;
    ERROR error = _SUCCESS;

    t = clock();
    f = fopen(*path, "rb");

    if(f) {
        path_RLE = add_extention(*path, RLE);
        if(path_RLE) {
            path_RLE_FREQ = add_extention(*path_RLE, FREQ);
            if(path_RLE_FREQ) {
                path_FREQ = add_extention(*path, FREQ);
                if(path_FREQ) {
                    n_blocks = fsize(f, *path, &block_size, &size_last);
                    size_f = (n_blocks - 1) * (int)block_size + (int)size_last;
                    if(size_f >= _1KiB) {
                        compressd = block_size;
                        size_rle = 0;
                        block_sizes = malloc(n_blocks * sizeof(unsigned long));
                        if(block_sizes) {
                            block_rle = malloc(n_blocks * sizeof(unsigned long));
                            if(block_rle) {
                                for(block_ID = 0, s = 0 ; block_ID < n_blocks ; ++block_ID) {
                                    if(block_ID == n_blocks - 1) {
                                        compressd = size_f - s;
                                    }
                                    block_sizes[block_ID] = compressd;
                                    buff = malloc(compressd * sizeof(uint8_t));
                                    if(buff) {
                                        if(fread(buff, sizeof(uint8_t), compressd, f) == compressd) {
                                            block = malloc(compressd * 2 + 3);
                                            if(block) {
                                                if(compress_rle) {
                                                    size_rle = block_compress(buff, block, compressd, size_f);
                                                    if(block_ID == 0) {
                                                        compress = compressd - size_rle;
                                                        ratio = (float)compress / (float)compressd;
                                                        if(ratio < 0.05 && !doRLE)
                                                            compress_rle = false; 
                                                    }
                                                }
                                                if(!f_RLE && compress_rle) {
                                                    f_RLE = fopen(path_RLE, "wb");
                                                    if(!f_RLE) {
                                                        error = _FILE_INACCESSIBLE;
                                                        break;
                                                    }
                                                }
                                                if(!f_RLE_FREQ && compress_rle) {
                                                    f_RLE_FREQ = fopen(path_RLE_FREQ, "wb");
                                                    if(!f_RLE_FREQ) {
                                                        error = _FILE_INACCESSIBLE;
                                                        break;
                                                    }
                                                }
                                                if(!f_FREQ && (doFREQ || (!compress_rle))) {
                                                    f_FREQ = fopen(path_FREQ, "wb");
                                                    if(!f_FREQ) {
                                                        error = _FILE_INACCESSIBLE;
                                                        break;
                                                    }
                                                }
                                                if(block_ID == 0 && compress_rle) {
                                                    print_rle = fprintf(f_RLE_FREQ, "@R@%lu", n_blocks);
                                                }
                                                if(block_ID == 0 && (!compress_rle || doRLE)) {
                                                    print = fprintf(f_FREQ, "@N@%lu", n_blocks);
                                                }
                                                if((print >= 4 && print_rle >= 4) || (print >= 4 && !compress_rle) || print_rle >= 4) {
                                                    unsigned *freq = malloc(sizeof(unsigned long) * 256);
                                                    if(freq) {
                                                        if(compress_rle) {
                                                            block_rle[block_ID] = size_rle;
                                                            int res = fwrite(block, 1, size_rle, f_RLE);
                                                            if(res == size_rle) {
                                                                make_freq(block, freq, size_rle);
                                                                if(fprintf(f_RLE_FREQ, "@%lu@", size_rle) >= 2) {
                                                                    error = write_freq(freq, f_RLE_FREQ, block_ID, n_blocks);
                                                                }
                                                                else
                                                                    error = _FILE_STREAM_FAILED;
                                                            }
                                                            else
                                                                error = _FILE_STREAM_FAILED;
                                                        }
                                                        if(!compress_rle || doFREQ) {
                                                            make_freq(buff, freq, compressd);
                                                            if(fprintf(f_FREQ, "@%lu@", compressd) >= 2) {
                                                                error = write_freq(freq, f_FREQ, block_ID, n_blocks);   
                                                            }
                                                            else
                                                                error = _FILE_STREAM_FAILED;
                                                        }
                                                        free(freq);
                                                    }
                                                    else
                                                        error = _NO_MEMORY;
                                                }
                                                else
                                                    error = _FILE_STREAM_FAILED;
                                                
                                                free(block);
                                            }
                                            else 
                                                error = _NO_MEMORY;
                                        }
                                        else
                                            error = _FILE_STREAM_FAILED;
                                        s += compressd;
                                        free(buff);
                                    }
                                    else
                                        error = _NO_MEMORY;
                                }
                                if(f_RLE)
                                    fclose(f_RLE);
                                if(f_FREQ)
                                    fclose(f_FREQ);
                                if(f_RLE_FREQ)
                                    fclose(f_RLE_FREQ);
                            }
                            else
                                error = _NO_MEMORY;
                        }
                        else 
                            error = _NO_MEMORY;
                    }
                    else
                        error = _FILE_TOO_SMALL;
                    if(error || (!doFREQ && (doRLE || compress_rle))) {
                        free(path_FREQ);
                        path_FREQ = NULL;
                    }
                }
                else
                    error = _NO_MEMORY;
                if(error || (!doRLE && !compress_rle)) {
                    free(path_RLE_FREQ);
                    path_RLE_FREQ = NULL;
                }
            }
            else
                error = _NO_MEMORY;
            if(error || (!compress_rle && !doRLE)) {
                free(path_RLE);
                path_RLE = NULL;
            }
        }
        else
            error = _NO_MEMORY;
        fclose(f);
    }
    else
        error = _FILE_INACCESSIBLE;
    if(!error) {
        if(compress_rle || doRLE) {
            free(*path);
            *path = path_RLE;
        }
        t = clock() - t;
        time = (float) ((((double) t) / CLOCKS_PER_SEC) * 1000);
        raphael(n_blocks, block_sizes, size_f, block_rle, time, path_RLE, path_FREQ, path_RLE_FREQ);
        free(block_sizes);
        free(block_rle);
        if(path_FREQ)
            free(path_FREQ);
        if(path_RLE_FREQ)
            free(path_RLE_FREQ);
    }
    return error;
}