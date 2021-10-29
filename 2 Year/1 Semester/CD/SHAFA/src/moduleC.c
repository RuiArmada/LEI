#include "../includes/moduleC.h"

uint8_t* coding(CODE_INDEX* table, uint8_t* in, unsigned long size, unsigned long* new_size) {
    CODE_INDEX* symbol;
    int nBytes, next = 0;
    uint8_t *code, *out;
    uint8_t* block_out = calloc(size * 1.05, sizeof(uint8_t));

    if(!block_out)
        return NULL;
    out = block_out;
    for(unsigned long id = 0 ; id < size ; ++id) {
        symbol = &table[next + *in++];
        nBytes = symbol->index;
        code = symbol->code;
        for(int i = 0 ; i < nBytes ; ++i)
            *out++ |= *code++;
        *out |= *code;
        next = symbol->next;
    }
    *new_size = out - block_out + (next ? 1 : 0);
    return block_out;
}



ERROR comp_buff(void* _args) {
    ARGS * args = (ARGS*) _args;
    char* codes = args->codes;
    uint8_t* input = args->input;
    unsigned long block_size = args->block_size;
    unsigned long* new_size = args->new_size;
    CODE_INDEX head, *symb_row;
    char current, next;
    int bit_ID, code_ID;
    uint8_t byte, mask, next_byte = 0;

    CODE_INDEX (*table)[NUM_SYMB] = calloc(1, sizeof(CODE_INDEX[OFFSET][NUM_SYMB]));

    if(!table)
        return _NO_MEMORY;
    current = *codes++;
    next = *codes++;
    for(int symb_ID = 0 ; symb_ID < NUM_SYMB ; ++symb_ID) {
        code_ID = 0;
        for( ; current != ';' && current != '\0' && code_ID < MAX_CODE_INT ; ++code_ID) {
            byte = 0;
            for(bit_ID = 0 ; bit_ID < 8 ; ++bit_ID) {
                if(current == '1')
                    ++byte;
                else if(current != '0') {
                    free(args->codes);
                    free(table);
                    return _FILE_UNRECOGNIZABLE;
                }
                if(bit_ID < 7) {
                    if(next == ';') {
                        byte <<= 7 - bit_ID;
                        current = next;
                        next = *codes++;
                        break;
                    }
                    else if(next == '\0') {
                        byte <<= 7 - bit_ID;
                        current = next;
                        break;
                    }
                    else
                        byte <<= 1;
                }
                current = next;
                next = *codes++;
            }
            table[0][symb_ID].code[code_ID] = byte;
        }
        if(code_ID > 0) {
            table[0][symb_ID].next = (bit_ID != 8) ? (bit_ID + 1) * NUM_SYMB : 0;
            table[0][symb_ID].index = code_ID - (bit_ID < 8 ? 1 : 0);
        }
        if(current != '\0') {
            current = next;
            next = *codes++;
        }
        else if(symb_ID < NUM_SYMB - 1) {
            free(args->codes);
            free(table);
            return _FILE_UNRECOGNIZABLE;
        }
    }
    free(args->codes);
    if(current != '\0') {
        free(table);
        return _FILE_UNRECOGNIZABLE;
    }
    for(int id = 0, new_id ; id < NUM_SYMB ; ++id) {
        head = table[0][id];
        if(head.next || head.index) {
            for(int offset = 1, bit_offset ; offset < OFFSET ; ++offset) {
                symb_row = &table[offset][id];
                bit_offset = (head.next / NUM_SYMB) + offset;
                new_id = head.index + (bit_offset < 8 ? 0 : 1);
                symb_row->index = new_id;
                symb_row->next = ((bit_offset < 8) ? bit_offset : bit_offset - 8) * NUM_SYMB;
                next_byte = 0;
                for(code_ID = 0 ; code_ID < new_id ; ++code_ID) {
                    byte = head.code[code_ID];
                    symb_row->code[code_ID] = (byte >> offset) | (next_byte << (8 - offset));
                    mask = (1 << offset) - 1;
                    next_byte = byte & mask;
                }
                if(bit_offset != 8) {
                    byte = head.code[new_id];
                    symb_row->code[new_id] = (byte >> offset) | (next_byte << (8 - offset));
                }
            }
        }
    }
    args->output = coding((CODE_INDEX*) table, input, block_size, new_size);
    free(table);
    if(!args->output)
        return _NO_MEMORY;
    
    return _SUCCESS;
}



ERROR write_SHAFA(void* _args, ERROR previous, ERROR error) {
    ARGS* args = (ARGS*) _args;
    FILE* f_SHAFA = args->f_SHAFA;
    uint8_t* output = args->output;
    unsigned long new_size = *args->new_size;

    if(!error) {
        if(!previous) {
            if(fprintf(f_SHAFA, "@%lu@", new_size) >= 2) {
                if(fwrite(output, sizeof(uint8_t), new_size, f_SHAFA) != new_size)
                    error = _FILE_STREAM_FAILED;
            }
            else
                error = _FILE_STREAM_FAILED;
        }
        free(output);
    }
    free(_args);

    return error;
}



void raphael(
                            unsigned long long n_blocks, 
                            unsigned long * input_size, 
                            unsigned long * output_size, 
                            double total_time,
                            char * path
                          ) {
    unsigned long b_input_size, b_output_size;
    printf("+------------------- Raphael -------------------+\n"
           "|        Module: C (Symbol Codes Codification)  |\n"
           "+-----------------------------------------------+"
           "-> Number of blocks: %lu                         \n", n_blocks);

    for(unsigned long long i = 0 ; i < n_blocks ; ++i) {
        b_input_size = input_size[i];
        b_output_size = output_size[i];
        printf("-> Size Before/After Compression (Block %lu): %lu%lu -> %d%%\n", i, b_input_size, b_output_size, (int) (((float) b_output_size / b_input_size) * 100));
    }
    printf(
        "-> Module Runtime (milliseconds): %f\n"
        "-> Generated File %s\n",
        total_time, path
    );
}




ERROR shafa(char** path) {
    FILE *f_FILE, *f_CODES, *f_SHAFA;
    ARGS* args;
    float total_time;
    char *path_CODES, *path_SHAFA, *block_COD, *path_FILE = *path;
    unsigned long num_block; // might be unsigned long long
    unsigned long size, *input_size, *output_size, *block_size = NULL;
    int error = _SUCCESS;
    uint8_t* input;
    
    clock_thread(START);
    path_CODES = add_extention(path_FILE, CODES);
    if(path_CODES) {
        f_CODES = fopen(path_CODES, "rb");
        if(f_CODES) {
            if(fscanf(f_CODES, "@%*c@%lu", &num_block) == 1) {
                f_FILE = fopen(path_FILE, "rb");
                if(f_FILE) {
                    path_SHAFA = add_extention(path_FILE, SHAFA);
                    if(path_SHAFA) {
                        f_SHAFA = fopen(path_SHAFA, "wb");
                        if(f_SHAFA) {
                            if(fprintf(f_SHAFA, "@%lu", num_block) >= 2) {
                                block_size = malloc(2 * num_block * sizeof(unsigned long));
                                if(block_size) {
                                    input_size = block_size;
                                    output_size = input_size + num_block;
                                    for(unsigned long long thread_ID = 0 ; thread_ID < num_block ; ++thread_ID) {
                                        block_COD = malloc((33151 + 1 + 1) * sizeof(char));
                                        if(!block_COD) {
                                            error = _NO_MEMORY;
                                            break;
                                        }
                                        if(fscanf(f_CODES, "@%lu@%33151[^@]", &size, block_COD) != 2) {
                                            free(block_COD);
                                            error = _FILE_STREAM_FAILED;
                                            break;
                                        }
                                        args = malloc(sizeof(ARGS));
                                        if(!args) {
                                            free(block_COD);
                                            error = _NO_MEMORY;
                                            break;
                                        }
                                        input = malloc(size * sizeof(uint8_t));
                                        if(!input) {
                                            free(block_COD);
                                            free(args);
                                            error = _NO_MEMORY;
                                            break;
                                        }
                                        if(fread(input, sizeof(uint8_t), size, f_FILE) != size) {
                                            free(block_COD);
                                            free(input);
                                            free(args);
                                            error = _FILE_STREAM_FAILED;
                                            break;
                                        }
                                        *args = (ARGS) {
                                            .block_size = size,
                                            .f_SHAFA = f_SHAFA,
                                            .codes = block_COD,
                                            .input = input,
                                            .output = NULL,
                                            .new_size = &output_size[thread_ID]
                                        };
                                        input_size[thread_ID] = size;
                                        error = thread_create(comp_buff, write_SHAFA, args);
                                        if(error) {
                                            free(block_COD);
                                            free(input);
                                            free(args);
                                            break;
                                        }
                                    }
                                    thread_wait();
                                } else 
                                    error = _NO_MEMORY;
                            } else 
                                error = _FILE_STREAM_FAILED;
                            
                            fclose(f_SHAFA);
                        } else 
                            error = _FILE_INACCESSIBLE;

                        if(error)
                            free(path_SHAFA);
                    } else 
                        error = _NO_MEMORY;

                    fclose(f_FILE);
                } else 
                    error = _FILE_INACCESSIBLE;
            } else 
                error = _FILE_UNRECOGNIZABLE;

            fclose(f_CODES);
        } else 
            error = _FILE_INACCESSIBLE;
        
        free(path_CODES);
    } else 
        error = _NO_MEMORY;
    
    if(!error) {
        *path = path_SHAFA;
        free(path_FILE);
        total_time = clock_thread(STOP);
        raphael(num_block, input_size, output_size, total_time, path_SHAFA);
    }
    if(block_size)
        free(block_size);

    return error;
}



