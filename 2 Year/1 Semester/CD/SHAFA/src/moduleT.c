#include "../includes/moduleT.h"

ERROR read_b(char* codes_input, unsigned long * freq) {
    int read_cout;

    if(sscanf(codes_input, "%lu%n", freq, &read_cout) != 1)
        return _FILE_UNRECOGNIZABLE;
    codes_input += read_cout + 1;
    for(int i = 1 ; i < NUM_SYMB ; i++) {
        if(sscanf(codes_input, "%lu%n", &freq[i], &read_cout) == 1) {
            if(codes_input[read_cout] != ";" && i != NUM_SYMB - 1)
                return _FILE_UNRECOGNIZABLE;
            codes_input += read_cout + 1;
        }
        else if(*codes_input == ";" || (*codes_input == '\0' && i == NUM_SYMB - 1)) {
            freq[i] = freq[i - 1];
            ++codes_input;
        } else
            return _FILE_UNRECOGNIZABLE;
    }
    if(codes_input[-1] != '\0')
        return _FILE_UNRECOGNIZABLE;

    return _SUCCESS;
}



void in_sort(unsigned long freq[], int pos[], int left, int right) {
    unsigned long tmp;
    int j, x, iter;

    for(int i = left + 1 ; i <= right ; ++i) {
        tmp = freq[i];
        j = i -1;
        while(j >= left && freq[i] < tmp) {
            freq[j + 1] = freq[j];
            --j;
        }
        freq[j + 1] = tmp;
        x = j + 1;
        iter = i - x;
        for(int id = i -1 ; iter ; --id)
            if(pos[id] >= x) {
                ++pos[id];
                --iter;
            }
        pos[i] = x;
    }
}



unsigned long sum_FREQ(unsigned long freq[], int first, int last) {
    unsigned long sum = 0;

    for(int i = first ; i <= last ; i++)
        sum += freq[i];
    return sum;
}



int best_div(unsigned long freq[], int first, int last) {
    int total, min_dif, dif, div = first;
    unsigned long aux = 0;

    total = min_dif = dif = sum_FREQ(freq, first, last);
    while(dif == min_dif) {
        aux = aux + freq[div];
        dif = abs(2 * aux - total);
        if(dif < min_dif) {
            div = div + 1;
            min_dif = dif;
        } else
            dif = min_dif + 1;
    }
    return div - 1; 
}



void add(char value, char codes[NUM_SYMB][NUM_SYMB], int start, int end) {
    char* symb_CODES;
    for( ; start <= end ; ++start) {
        symb_CODES = codes[start];
        for( ; *symb_CODES ; ++symb_CODES);
        *symb_CODES = value;
    }
}



void shafa_CODES(unsigned long freq[], char codes[NUM_SYMB][NUM_SYMB], int start, int end) {
    if(start != end) {
        int div = best_div(freq, start, end);
        add('0', codes, start, div);
        add('1', codes, div + 1, end);
        shafa_CODES(freq, codes, start, div);
        shafa_CODES(freq, codes, div + 1, end);
    }
}



int null(unsigned long freq[NUM_SYMB]) {
    int r = 0;
    for(int i = NUM_SYMB ; freq[i] == 0 ; --i)
        ++r;
    return (NUM_SYMB - 1 - r);
}



void raphael(
                            unsigned long long n_blocks, 
                            unsigned long * sizes,  
                            double total_time,
                            char * path
                          ) {
    unsigned long long i;
    printf("+------------------- Raphael -------------------+\n"
           "|      Module: C (Calculation of Symbol Codes)  |\n"
           "+-----------------------------------------------+"
           "-> Number of blocks: %lu                         \n", n_blocks);

    for(i = 0 ; i < n_blocks - 1 ; ++i) {
        printf("-> Size of Block: %lu/", sizes[i]);
    }
    printf("%lu bytes\n", sizes[i]);
    printf("-> Module Runtime (milliseconds): %f\n"
           "-> Generated File: %s\n",
           total_time, path
           );
}



ERROR get_CODES(char * path) {
    clock_t time;
    FILE *f_FREQ, *f_CODES;
    char mode, *path_FREQ, *path_CODES, *input;
    unsigned long long num_blocks = 0;
    unsigned long size = 0, freq[NUM_SYMB], *sizes = NULL;
    int freq_not, iter, error = _SUCCESS, pos[NUM_SYMB];
    double total_time;
    char (*codes)[NUM_SYMB];

    path_FREQ = add_extention(path, FREQ);
    if (path_FREQ) {
        f_FREQ = fopen(path_FREQ, "rb");
        if (f_FREQ) {
            if (fscanf(f_FREQ, "@%c@%lu", &mode, &num_blocks) == 2) {   
                if (mode == 'R' || mode == 'N') {
                    sizes = malloc(num_blocks * sizeof(unsigned long));
                    if (sizes) {                    
                        path_CODES = add_extention(path, CODES);
                        if (path_CODES) {
                            f_CODES = fopen(path_CODES, "wb");
                            if (f_CODES) {
                                    if (fprintf(f_CODES, "@%c@%lu", mode, num_blocks) >= 3) {                                                                   
                                    for (long long i = 0; i < num_blocks && !error; ++i) {
                                        codes = calloc(1, sizeof(char[NUM_SYMB][NUM_SYMB]));
                                        if (codes) {
                                            memset(freq, 0, NUM_SYMB * 4);
                                            for (int j = 0; j < NUM_SYMB; ++j) pos[j] = j;
                                            if (fscanf(f_FREQ, "@%lu", &size) == 1) {
                                                sizes[i] = size;
                                                input = malloc(9 * NUM_SYMB + (NUM_SYMB - 1) + 1); 
                                                if (input) {
                                                    if (fscanf(f_FREQ, "@%2559[^@]", input) == 1) {
                                                        error = read_b(input, freq);
                                                        if (!error) {
                                                            in_sort(freq, pos, 0, NUM_SYMB - 1);
                                                            freq_not = null(freq);
                                                            shafa_CODES(freq, codes, 0, freq_not);
                                                            if (fprintf(f_CODES, "@%lu@", size) >= 2) {
                                                                for (iter = 0; iter < NUM_SYMB - 1 && !error; ++iter) {
                                                                    if (fprintf(f_CODES, "%s;", codes[pos[iter]]) < 1)
                                                                        error = _FILE_STREAM_FAILED;
                                                                }
                                                                if (!error && fprintf(f_CODES, "%s", codes[pos[iter]]) < 0) 
                                                                    error = _FILE_STREAM_FAILED;
                                                            }
                                                            else 
                                                                error = _FILE_STREAM_FAILED;                                
                                                        }
                                                    }
                                                    else 
                                                        error = _FILE_STREAM_FAILED;
                                                    free(input);
                                                }
                                                else
                                                    error = _NO_MEMORY;
                                            }
                                            else 
                                                error = _FILE_STREAM_FAILED;
                                            free(codes);
                                        }
                                        else
                                            error = _NO_MEMORY;
                                    }
                                }
                                else 
                                    error = _FILE_STREAM_FAILED;

                                if (!error)
                                    fprintf(f_CODES, "@0");

                                fclose(f_CODES);
                            }
                            else {
                                error = _FILE_INACCESSIBLE;

                                free(path_CODES);
                            }
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
                error = _FILE_UNRECOGNIZABLE;

            fclose(f_FREQ);
        }
        else 
            error = _FILE_INACCESSIBLE;

        free(path_FREQ);
    }
    else
        error = _NO_MEMORY;     

    if (!error) {
        time = clock() - time;
        total_time = (((double) time) / CLOCKS_PER_SEC) * 1000;

        raphael(num_blocks, sizes, total_time, path_CODES);
    }              

    free(sizes);
    
    return error;
}