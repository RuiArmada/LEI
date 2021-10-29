#include "../includes/file.h"

long long fsize(FILE *fp_in, char *filename, unsigned long *block_size, long *size_last_block) {
    unsigned long long total;
    long long n_blocks;
    unsigned long n_read, block_Size;
    unsigned char *temp_buff;
    int fseek_error;
    FILE *fp;
    block_Size = *block_size;

    if(block_Size > FSIZE_MAX_BLOCK_SIZE)
        return FSIZE_ERROR_BLOCK_SIZE;
    if(block_Size == 0UL)
        block_Size = FSIZE_DEFAULT_BLOCK_SIZE;
    if(block_Size < FSIZE_MIN_BLOCK_SIZE)
        block_Size = FSIZE_MIN_BLOCK_SIZE;

    *block_size = block_Size;

    if(filename == NULL || *filename == 0)
        fp = fp_in;
    else {
        fp = fopen(filename, "rb");
        if(fp == NULL)
            return FSIZE_ERROR_IN_FILE;
    }

    fseek_error = (fseek(fp, 0L, SEEK_SET));
    if(fseek_error)
        return FSIZE_ERROR_IN_FILE;

    fseek_error = fseek(fp, 0L, SEEK_END);
    if(!fseek_error) {
        total = ftell(fp);
        if(total == FSIZE_ERROR_IN_FTELL)
            return FSIZE_ERROR_IN_FILE;
        n_blocks = total / block_Size;
        if(n_blocks * block_Size == total)
            *size_last_block = block_Size;
        else {
            *size_last_block = total - n_blocks * block_Size;
            n_blocks++;
        }
        fseek_error = fseek(fp, 0L, SEEK_SET);
        if(fseek_error)
        	return FSIZE_ERROR_IN_FILE;   
        else
            return n_blocks;
    }

    n_blocks = FSIZE_MAX_SIZE_FSEEK / block_Size - 1;

    fseek_error = fseek(fp, n_blocks * block_Size, SEEK_SET);
    if(fseek_error)
        return FSIZE_ERROR_IN_FILE;
    temp_buff = malloc(sizeof(unsigned char) * block_Size);

    while(n_read == block_Size && n_blocks <= FSIZE_MAX_NUMBER_OF_BLOCKS) {
        n_blocks++;
        n_read = fread(temp_buff, sizeof(unsigned char), block_Size, fp);
    }

    free(temp_buff);

    if(n_blocks > FSIZE_MAX_NUMBER_OF_BLOCKS)
        return FSIZE_ERROR_NUMBER_OF_BLOCKS;

    if(n_read == 0L) {
        *size_last_block = block_Size;
        n_blocks--;
    }
    else
        *size_last_block = n_read;

    if(filename == NULL || *filename == 0) {
        fseek_error = fseek(fp, 0L, SEEK_SET);
        if(fseek_error)
            return FSIZE_ERROR_IN_FILE;
    }
    else
        fclose(fp);
        
    return n_blocks;
}