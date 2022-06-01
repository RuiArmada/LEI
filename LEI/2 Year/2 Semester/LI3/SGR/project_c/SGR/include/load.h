/**    
 * @file load.h    
 * @author Miguel Gomes, Ariana Lousada, Rui Armada.   
 * @date 26 Abril 2021
 * @brief Load files methods.   
 */ 
#ifndef __LOAD_H__
#define __LOAD_H__


#include "SGR.h"
#include <stdio.h>

/**
 * @brief Cleans '\0' os a string
 * @param s string to be cleaned
 * @param size size of string
*/
void clean(char* s,size_t size);

/**
 * @brief Loads Users from CSV file
 * @param d SGR Structure
 * @param fp file to be parsed
*/
void load_users_from_csv (SGR d,FILE *fp);

/**
 * @brief Loads Business from CSV file
 * @param d SGR Structure
 * @param fp file to be parsed
*/
void load_business_from_csv (SGR d,FILE *fp);

/**
 * @brief Loads Reviews from CSV file
 * @param d SGR Structure
 * @param fp file to be parsed
*/
void load_reviews_from_csv (SGR d,FILE *fp);

#endif /*LOAD_H*/
