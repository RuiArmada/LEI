/**    
 * @file auxiliar.h    
 * @author Miguel Gomes, Ariana Lousada, Rui Armada.   
 * @date 26 Abril 2021
 * @brief Auxiliar functions.   
 */ 
#ifndef __UTILS_H__
#define __UTILS_H__

#include "SGR.h"
#include "structs.h"

enum OPERATOR{LT,EQ,GT};

/**
 * @brief Counts the number of Chars
 * @param d pointer to a String
*/
int charcount(char* d);

/**
 * @brief Turns all characters to Lower Case
 * @param s pointer to a String
*/
char* toLow(char* s);

/**
 * @brief Checks if a word is on a String
 * @param word word to be found
 * @param sring string to be searched
*/
int find_word_in_string(char* word, char* string);

/**
 * @brief Checks if a word is on a text of a Review
 * @param search_word word to be found
 * @param text Review Text to be searched
*/
int findWordR(char* search_word, char* text);

/**
 * @brief Checks if a word is on any Review
 * @param rev pointer to Review structure
 * @param word word to be found
*/
char* findR(reviews* rev, char* word);

/**
 * @brief Compares Id
 * @param id ID to be compared
 * @param val Value to compare with 
 * @param i Operator
*/
int comp_ids(char* id, char* val, enum OPERATOR i);

/**
 * @brief Creates a new review of a Business
 * @param name name of Business Reviewd
 * @param id ID of Business Reviewd
 * @param city City of Business Reviewd 
 * @param state State of Business Reviewd
 * @param n_rev Review
 * @param stars Stars to give
 * @param cat Category of Business Reviewd
*/
business* new_b_l(char* name,char* id,char* city,char* state,size_t n_rev,float stars,char* cat);

/**
 * @brief Initiates a City Structure
*/
city_arr* init_city();

/**
 * @brief Prints a array of cities
 * @param c pointer to the array
*/
void print_ct_arr(city_arr* c);

/**
 * @brief inserts a city into the array
 * @param c pointer to the array
 * @param city city to be inserted
*/
void insert_city(city_arr* c, char* city);

/**
 * @brief Sorts the Insert
 * @param head_ref
 * @param new_node
*/
void sortedInsert(struct top_arr** head_ref,struct top_arr* new_node);

/**
 * @brief Inserts a Business of a given City
 * @param c pointer to city_arr
 * @param b business to be inserted
*/
void insert_buz(city_arr* c, business* b);

/**
 * @brief Show business per city
 * @param d SGR structure
*/
city_arr* per_city(SGR d);

/**
 * @brief Show business per category
 * @param d SGR structure
 * @param category selected category
*/
city_arr* per_cat(SGR d,char* category);

/**
 * @brief Hashes the Businesses
 * @param state
*/
int hashingB(char* state);

/**
 * @brief Inserts Category
 * @param c city_arr
 * @param b business
*/
void insert_cat(city_arr* c, business* b);

#endif /*UTILS_H*/
