/**    
 * @file SGR.h    
 * @author Miguel Gomes, Ariana Lousada, Rui Armada.   
 * @date 26 Abril 2021
 * @brief Querys and other SGR methods.   
 */ 
#ifndef __SGR_H__
#define __SGR_H__

#include "structs.h"
typedef struct sgr{
	user_cat * user;
	business_cat * business;
	reviews_cat * reviews;
}* SGR;

/**
 * @brief Initiates the SGR Object
*/
SGR init_sgr();

/**
 * @brief Destroys the SGR Object
 * @param d SGR Object
*/
void free_sgr(SGR d);

/**
 * @brief Query 1 - Loads the User, Business and Review Data from a CSV File
 * @param use_f User file path
 * @param bus_f Business file path
 * @param rev_f Review file path
*/
SGR load_sgr(char* use_f, char* bus_f, char* rev_f);

/**
 * @brief Query 2 - Gets a Business started by a given letter
 * @param d SGR Object
 * @param letter letter to be searched
*/
TABLE businesses_started_by_letter(SGR d,char letter);

/**
 * @brief Query 3 - Loads a Business Info given it's ID
 * @param srg SGR Object
 * @param id id of wanted Business 
*/
TABLE business_info(SGR srg, char *id);

/**
 * @brief Query 4 - Businesses reviewed by a certain User
 * @param sgr SGR Object
 * @param user_id id of user
*/
TABLE businesses_reviewed(SGR sgr, char * user_id);

/**
 * @brief Query 5 - Searches Businesses with a given number of stars and of a given a city
 * @param sgr SGR Object
 * @param stars number os stars
 * @param city city of wanted businesses
*/
TABLE businesses_with_stars_and_city(SGR sgr,float stars, char * city);

/**
 * @brief Query 6 - TOP n Businesses by city
 * @param d SGR Object
 * @param top number of Businesses to display
*/
TABLE top_businesses_by_city(SGR d, int top);

/**
 * @brief Query 7 - Gets International Users
 * @param sgr SGR Object
*/
TABLE international_users(SGR sgr);

/**
 * @brief Query 8 - TOP n Businesses of a given Category
 * @param d SGR Object
 * @param top number o Businesses to display
 * @param category wanted category
*/
TABLE top_businesses_with_category(SGR d, int top, char* category);

/**
 * @brief Query 9 - Searches for Reviews with a given word in the TEXT section
 * @param sgr SGR Object
 * @param word word to be searched
*/
TABLE reviews_with_word(SGR sgr, char * word);

#endif /*SGR_H*/
