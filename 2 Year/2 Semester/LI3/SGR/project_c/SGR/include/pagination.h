/**    
 * @file pagination.h    
 * @author Miguel Gomes, Ariana Lousada, Rui Armada.   
 * @date 26 Abril 2021
 * @brief Pagination.   
 */ 
#ifndef __PAGINATION_H__
#define __PAGINATION_H__


#include "structs.h"
#include <stdarg.h>


#define rows 24

/**
 * @brief to_list2 
 * @param t TABLE2 Object
 * @param size size of Object
*/
TABLE2 **to_list2(TABLE2 * t,size_t size);

/**
 * @brief Prints Business Info page
 * @param page TABLE2 Objects
*/
void print_page_2(TABLE2* page);

/**
 * @brief to_list
 * @param t TABLEIDS Object
 * @param size size of Object
*/
TABLEIDS **to_list(TABLEIDS * t,size_t size);

/**
 * @brief Prints the Page ID
 * @param page TABLEIDS Object
*/
void print_page_id(TABLEIDS* page);

/**
 * @brief Pagination Method
 * @param t TABLE Object
*/
void pages(TABLE t);

#endif /*PAGINATION_H*/
