/**    
 * @file hash.h    
 * @author Miguel Gomes, Ariana Lousada, Rui Armada.   
 * @date 26 Abril 2021
 * @brief Auxiliar structures.   
 */ 
#ifndef __HASH_H__
#define __HASH_H__


#include "SGR.h"
#include "structs.h"

/**
 * @brief Determines size od a String
 * @param s string to be sized
*/
size_t long_size(char* s);

/**
 * @brief Hash
 * @param s const char
*/
size_t hash(const char *s);

/**
 * @brief Creates a new User
 * @param nome name of User
 * @param id ID of User
*/
user* new_u(char* nome,char* id);

/**
 * @brief Creates a new Business
 * @param nome name of Business
 * @param id ID of Business
 * @param city City of Business
 * @param state State of Business
 * @param cat Category of Business
*/
business* new_b(char* nome,char* id,char* city,char* state,char* cat);

/**
 * @brief Creates a new Review
 * @param id ID of Review
 * @param b_id ID of Business Reviewed 
 * @param u_ID ID of User Reviewing
 * @param stars Stars of the Review
 * @param text Commentary of the Review
*/
reviews* new_r(char* id,char* b_id,char* u_id,float stars,char* text);

/**
 * @brief Inserts User in hashtable
 * @param d SGR structure var
 * @param u pointer to User structure
*/
void insert_hash_user(SGR d,user* u);

/**
 * @brief Inserts Business in hashtable
 * @param d SGR structure var
 * @param b pointer to Business structure
*/
void insert_hash_business(SGR d, business* b);

/**
 * @brief Inserts Review in hashtable
 * @param d SGR structure var
 * @param r pointer to Review structure
*/
int insert_hash_review(SGR d,reviews* r);

#endif /*HASH_H*/
