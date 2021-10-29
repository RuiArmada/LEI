/**    
 * @file interpreter.h    
 * @author Miguel Gomes, Ariana Lousada, Rui Armada.   
 * @date 26 Abril 2021
 * @brief Interpreter.   
 */ 
#ifndef __INTERPRETER_H__
#define __INTERPRETER_H__

#include "../include/structs.h"
#include "../include/SGR.h"

/**
 * @brief Removes quotation marks
 * @param s string to be operated in
*/
char* remove_quotes(char* s);

/**
 * @brief STRMBTOK
 * @param input input command
 * @param delimit delimiter
 * @param openblock
 * @param closeblock
*/
char *strmbtok ( char *input, char *delimit, char *openblock, char *closeblock);

/**
 * @brief Splits a string at a given place
 * @param string string to be operated in
 * @param del delimiter
*/
char** split_at(char* string,char* del);

/**
 * @brief Gets Columns
 * @param value char value
*/
int get_col(char* value);

/**
 * @brief Frees the TABLEIDS Object
 * @param head head of TABLEIDS Object
*/
void freeList_ids(struct TABLEIDS* head);

void show(TABLE t);

/**
 * @brief Frees the TABLE2 Object
 * @param head head of TABLE2 Object
*/
void freeList_2(struct TABLE2* head);

/**
 * @brief Frees the TABLE Object
 * @param head head of TABLE Object
*/
void free_table(TABLE t);

/**
 * @brief reverse
 * @param s[] to be reversed
*/
void reverse(char s[]);

/**
 * @brief itoa
 * @param n int
 * @param s[] char
*/
void itoa(int n, char s[]);

/**
 * @brief WriteFrmtd
 * @param stream FILE *
 * @param format char *
*/
void WriteFrmtd(FILE *stream, char *format, ...);

/**
 * @brief atribute the content of the CSV FIle to a variable
 * @param path path of the file
 * @param del delimiter
*/
TABLE fromCSV(char* path,char del);

/**
 * @brief access values at a given position
 * @param t TABLE Object
 * @param x X Value
 * @param y Y Value
*/
TABLE get_pos(TABLE t,int x, int y);

/**
 * @brief Sends variable content to a CSV File
 * @param TABLE Object
 * @param del delimiter
 * @param path path to the CSV File
*/
void toCSV(TABLE t,char del,char* path);

/**
 * @brief prints help menu
*/
void print_help();

/**
 * @brief pops up a prompt
*/
void prompt();

/**
 * @brief main loop
 * @param u char* of users
 * @param b char* of businesses
 * @param r char* of reviews
*/
void loop(char* u,char* b,char* r);

#endif /*__INTERPRETER_H__*/
