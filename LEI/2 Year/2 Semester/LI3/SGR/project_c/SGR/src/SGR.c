
#include "../include/structs.h"
#include "../include/SGR.h"
#include "../include/load.h"
#include "../include/auxiliar.h"
#include "../include/hash.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

SGR init_sgr(){
	SGR d = malloc(sizeof(SGR));
	d->user = malloc(sizeof(user_cat));
	d->business = malloc(sizeof(business_cat));
	d->reviews = malloc(sizeof(reviews_cat));
	d->user->n_users=0;
	d->business->n_business=0;
	d->reviews->n_reviews=0;
    	for (int i = 0;i<MAX_TABLE;i++){
		d->user->hash_table_user[i] = malloc(sizeof(l_user));
		d->business->hash_table_business[i] = malloc(sizeof(l_business));
		d->reviews->hash_table_review[i] = malloc(sizeof(l_review));
		d->business->hash_table_business[i]->business = NULL;
		d->user->hash_table_user[i]->user = NULL;
		d->reviews->hash_table_review[i]->reviews = NULL;
		d->user->hash_table_user[i]->head = NULL;
		d->user->hash_table_user[i]->last = NULL;
		d->business->hash_table_business[i]->head = NULL;
		d->business->hash_table_business[i]->last = NULL;
		d->reviews->hash_table_review[i]->head = NULL;
		d->reviews->hash_table_review[i]->last = NULL;
	}
	return d;
}

void free_sgr(SGR d){
	user *u,*tmp_u;
	business *b,*tmp_b;
	reviews *r,*tmp_t;
	for(int i = 0;i<MAX_TABLE;i++){
		 u = d->user->hash_table_user[i]->head;
		 r = d->reviews->hash_table_review[i]->head;
		 b = d->business->hash_table_business[i]->head;
		 while(u != NULL){
			 tmp_u = u;
			 u = u->next;
			 free(tmp_u);
		 }
		 while(b != NULL){
			 tmp_b = b;
			 b = b->next;
			 free(tmp_b);
		 }
		 while(r != NULL){
			 tmp_t = r;
			 r = r->next;
			 free(tmp_t);
		}
	}
	free(d->user);
	free(d->reviews);
	free(d->business);
	free(d);
}

/* Query 1
 *
 *SGR load_sgr(path_to_user, path_to_business, path_to_reviews);
 *
 */

SGR load_sgr(char* use_f, char* bus_f, char* rev_f){
	SGR d = init_sgr();
	FILE *fp_u = fopen (use_f, "r"), *fp_b = fopen (bus_f, "r"), *fp_r = fopen (rev_f, "r");

	if (!(fp_u && fp_b && fp_r)) {
		return NULL;
	}
    	load_users_from_csv(d,fp_u);
		
    	load_business_from_csv(d,fp_b);
		
    	load_reviews_from_csv(d,fp_r);

		
		fclose(fp_u);
		fclose(fp_b);
		fclose(fp_r);
	return d;
}

/* Query 2
 *
 *TABLE businesses_started_by_letter(SGR, letter);
 *
 */

TABLE businesses_started_by_letter(SGR d,char letter){    
        letter = tolower(letter);    
        business* temp;    
        TABLE out = emptyTable();    
        for (int i = 0;i<MAX_TABLE;i++){    
                temp = d->business->hash_table_business[i]->head;    
                while(temp){    
                        char* name = get_bus_name(temp);    
                        if(tolower(name[0]) == letter){    
                                insert_table(temp, out);    
                        }    
                        temp = temp->next;    
                }    
        }    
        return out;    
}

/* Query 3
 *
 *TABLE business_info(SGR, business_id);
 *
 */

TABLE business_info(SGR srg, char *id) {
	TABLE t = emptyTable();
	size_t slot = hash(id);
    	business * bus = srg->business->hash_table_business[slot]->business;
    	if (!bus) {
		return NULL; 
	}
    	while (bus){
        	if(!strcmp(bus->business_id, id)){
			insert_table(bus,t);
			break;
		}
		bus = bus->next;
	}
	return t;
}

/* Query 4
 *
 *TABLE businesses_reviewed(SGR, user_id);
 *
 */

TABLE businesses_reviewed(SGR sgr, char * user_id){
	size_t slot = hash(user_id);
	TABLE result = emptyTable();
	
	reviews * review = sgr->reviews->hash_table_review[slot]->reviews;
	
	while(review) {
		if (!strcmp(review->user_id, user_id)){
			size_t slot2 = hash(review->business_id);
			business * b = sgr->business->hash_table_business[slot2]->business;    
			while(b){
				if(!strcmp(b->business_id,review->business_id)){
					insert_table(b, result);
				}
				b = b->next;
			}
		}
		review = review->next;
	}
	return result;
}

/* Query 5
 * 
 *TABLE businesses_with_stars_and_city(sgr, stars, city);
 *
 */

TABLE businesses_with_stars_and_city(SGR sgr,float stars, char * city){
	TABLE res = emptyTable();
	char name[MAX_NAME+1];
	char name2[MAX_NAME+1];
	strcpy(name2,city);
	business * bus;
	for(size_t slot=0; slot<MAX_TABLE; slot++){ 
		bus = sgr->business->hash_table_business[slot]->business;
		if(bus){
			for(int i = 0;i<MAX_NAME;i++){
				name[i] = tolower(bus->business_city[i]);
				name2[i] = tolower(name2[i]);
			}
			float st = bus->m_stars;
			if(st>=stars && !strcmp(name,name2)){
				insert_table(bus,res);
			}
			bus = bus->next;
		}
	}
	return res;
}

/* Query 6
 *
 *TABLE top_businesses_by_city(SGR d, int top); 
 *
 */

TABLE top_businesses_by_city(SGR d, int top){
	TABLE t = emptyTable();
	city_arr* c = per_city(d);
	top_arr* at;
	int i;
	for(;c!=NULL;c=c->next){
		i = 0;
		at = c->top;
		for(;at!=NULL && i < top;i++,at=at->next)
			insert_table(at->b_city,t);
	}
	return t;
}

/* Query 7
 *
 *TABLE international_users(SGR d); 
 *
 */

TABLE international_users(SGR d){
	reviews * rev; TABLE res = emptyTable(); business *bus1, *bus2=NULL; user* usr;
	char uid[MAX_ID+1],bid[MAX_ID+1];
	for(int i = 0;i<MAX_TABLE;i++){
		usr = d->user->hash_table_user[i]->user;
		while(usr!=NULL){
			strcpy(uid,usr->user_id);
			rev = d->reviews->hash_table_review[hash(uid)]->head;
			while(rev!=NULL){
				if(!(strcmp(uid,rev->user_id))){
					strcpy(bid,rev->business_id);
					bus1 = d->business->hash_table_business[hash(bid)]->business;
					while(bus1 !=NULL){
						if(!(strcmp(bid, bus1->business_id))){
							if (bus2 != NULL && strcmp(bus2->business_id,bus1->business_id)){
								if(strcmp(bus2->business_state, bus1->business_state)){
									insert_id_table(usr->user_id, res);
									goto nextu;
								}
							}
							bus2 = bus1;
						}
						bus1 = bus1->next;
					}
				}
				rev=rev->next;
			}
nextu:
			usr = usr->next;
		}
	}
	return res;
}

/* Query 8
 *
 *TABLE top_businesses_with_category(SGR d, int top, char* category); 
 *
 */

TABLE top_businesses_with_category(SGR d, int top, char* category){
	TABLE t = emptyTable();
	city_arr* c = per_cat(d, category);
	top_arr* tr = c->top;
	for(int i=0;tr!=NULL && i<top;i++,tr=tr->next)
		insert_table(tr->b_city,t);
	return t;
}

/* Query 9
 *
 *TABLE reviews_with_word(SGR d, char* word);
 *
 */

TABLE reviews_with_word(SGR sgr, char * word){
	TABLE table = emptyTable();
	reviews *rev;
	for(int i = 0; i<MAX_TABLE; i++){
		rev = sgr->reviews->hash_table_review[i]->reviews;
		while(rev){
			char* r_id = findR(rev,word);
			if(r_id != NULL){
				insert_id_table(r_id,table);
			}
			rev = rev->next;
		}
	}
	return table;
}
