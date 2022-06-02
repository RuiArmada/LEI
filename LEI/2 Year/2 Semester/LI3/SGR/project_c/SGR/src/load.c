#include "../include/structs.h"
#include "../include/SGR.h"
#include "../include/utils.h"
#include "../include/hash.h"
#include <stdlib.h>

void clean(char* s,size_t size){
	size_t i=0;
	while(i<size){
		s[i]='\0';
		i++;
	}
}


void load_users_from_csv (SGR d,FILE *fp){
	char buf[CATEGORIES], *name = malloc(30), *id = malloc(22);
	while (fgets (buf, CATEGORIES, fp)) {
		clean(name,30);
		clean(id,23);
		if (sscanf (buf, "%22[^;];%29[^;];\n",
					id,name) == 2) {
			insert_hash_user(d,new_u(name,id));
		}
	}
	free(name);
	free(id);
	printf("USERS LOADED\n");
}

void load_business_from_csv (SGR d,FILE *fp){
	char buf[CATEGORIES], *name = malloc(30), *id = malloc(23), *city = malloc(23), *state = malloc(23), *categories = malloc(128);

	while (fgets (buf, CATEGORIES, fp)) {
		clean(id,23);
		clean(name,30);
		clean(city,23);
		clean(state,23);
		clean(categories,128);
		if (sscanf (buf, "%22[^;];%29[^;];%22[^;];%22[^;];%127[^;]\n",
					id,name,city,state,categories) == 5) {
			insert_hash_business(d,new_b(name,id,city,state,categories));
		}
	}
	free(id);
	free(name);
	free(city);
	free(state);
	free(categories);
	printf("BUSINESS LOADED\n");
}

void load_reviews_from_csv (SGR d,FILE *fp){
	char buf[7000], *r_id = malloc(23), *b_id = malloc(23), *u_id = malloc(23),*text=malloc(6000);
	float stars;

	while (fgets (buf, BUF, fp)) {
		clean(r_id,23);
		clean(u_id,23);
		clean(b_id,23);
		clean(text,6000);
		if (sscanf (buf, "%22[^;];%22[^;];%22[^;];%f;%*f;%*f;%*f;%*[^;];%5999[^\n]",
					r_id,u_id,b_id,&stars,text) == 5){
			insert_hash_review(d,new_r(r_id,b_id,u_id,stars,text));
			
			
			
			
		}
	}
	free(r_id);
	free(u_id);
	free(b_id);
	free(text);
	printf("REVIEWS LOADED\n");
}
