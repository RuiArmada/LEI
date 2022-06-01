/**    
 * @file structs.h    
 * @author Miguel Gomes, Ariana Lousada, Rui Armada.   
 * @date 26 Abril 2021
 * @brief Structures definitions.   
 */ 
#ifndef __STRUCTS_H__
#define __STRUCTS_H__

#include "includes.h"
#include "macro.h"

typedef struct reviews { //id, userID, businessID, stars, funny, cool, text    
	char* review_id;
	char* user_id;
	char* business_id;
	float stars;
	char * text;
	struct reviews *next;
} reviews;

typedef struct l_review {
    reviews* reviews;
    struct reviews *head;
    struct reviews *last;
}l_review;

typedef struct business { //id, name, city, state, categories    
        char* business_id;    
        char* business_name;    
        char* business_city;    
        char* business_state;    
        char* business_cat;
	size_t n_reviews;
	float m_stars;
        struct business *next;
} business;

typedef struct l_business {
    business* business;
    struct business *head;
    struct business *last;
}l_business;

typedef struct user { //id, name, friends
    char* user_id;
    char* user_name;
    size_t n_reviews;
    struct user *next;
}user;

typedef struct l_user {
    user* user;
    struct user *head;
    struct user *last;
}l_user;

typedef struct business_cat {
    l_business* hash_table_business[MAX_TABLE];
    size_t n_business;

}business_cat;

typedef struct user_cat {
    l_user* hash_table_user[MAX_TABLE];
    size_t n_users;

}user_cat;

typedef struct reviews_cat {
    l_review * hash_table_review[MAX_TABLE];
    size_t n_reviews;
}reviews_cat;

typedef struct TABLE2 {
    business * info;
    struct TABLE2 * next;

}TABLE2;

typedef struct TABLEIDS {
    char * id;
    struct TABLEIDS * next;
}TABLEIDS;

typedef struct {
    TABLE2 * table;
    TABLE2 * last;
    TABLEIDS *ids;
    TABLEIDS * lastID;
}*TABLE;

typedef struct top_arr{
	business* b_city;
	struct top_arr *next;
}top_arr;

typedef struct city_arr{
	char * city;
	top_arr* top;
	top_arr* last;
	struct city_arr *next;
}city_arr;

/**
 * @brief Constructs a Empty Table
*/
TABLE emptyTable();

/**
 * @brief Prints Table Object to the screen
 * @param t Table Object
*/
void printTable(TABLE t);

/**
 * @brief Inserts Business into Table Object
 * @param b Business to be inserted
 * @param t Table Object
*/
void insert_table(business* b, TABLE t);

/**
 * @brief Inserts Review_ID into Table Object
 * @param b Review_ID to be inserted
 * @param t Table Object
*/
void insert_id_table(char* r_id, TABLE t);

#endif /*STRUCTS_H*/
