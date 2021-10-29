#include <string.h>
#include <stdlib.h>
#include "../include/hash.h"
#include "../include/auxiliar.h"
#include <ctype.h>

int charcount(char* d){
	int count = 0;
	for(int i = 0;d[i]!='\n';i++,count++);
	return count;
}

char* toLow(char* s){
	char * out = malloc(sizeof(char)*long_size(s)+1);
	int i=0;
	while(s[i]){
		out[i] = tolower(s[i]);
		i++;
	}
	return out;
}

int find_word_in_string(char* word, char* string){
    char *lower_word = malloc(sizeof(char)*long_size(word)+1);
    char *lower_string = malloc(sizeof(char)*long_size(string)+1);
    char *ret;
    int r;

    strcpy(lower_word,toLow(word));
    strcpy(lower_string,toLow(string));


    ret = strstr(lower_string, lower_word);
    if (ret)
	    r=1;
    else
	    r=0;
    free(lower_string);
    free(lower_word);
    return r;
}

int findWordR(char*searchW, char*text){
    int count = 0;
    int j = 0;
    for (int i = 0; count==0 && text[i]!='\0'; i++){
        
        if(isalpha(text[i])){ 
            if(searchW[j]==text[i]){
                if((isspace(text[i+1]) || ispunct(text[i+1])) || searchW[j+1] == '\0'){
                    count++;
                    return 1;
                }
                j++;
            } else {
                j=0;
                while(isalpha(text[i])){
                    i++;
                }
            }  
        }
    }
   return 0;
}

char *findR(reviews* review, char *word){
    char* text1 = malloc(sizeof(char)*long_size(review->text)+1);
    strcpy(text1,review->text);
    if(review->text != NULL){
        if(findWordR(word,text1) == 1){
        return review->review_id;
        } 
    }
    free(text1);
    return NULL;
}

int comp_ids(char* id, char* val, enum OPERATOR i){
	switch(i){
	case LT:
		return(strcmp(id, val)<0);
	case GT:
		return(strcmp(id, val)>0);
	case EQ:
		return(strcmp(id, val)==0);
	default:
		return 0;
	}
}

business* new_b_l(char* name,char* id,char* city,char* state,size_t n_rev,float stars,char* cat){
	size_t name_s =long_size(name)+1, id_s = long_size(id)+1, city_s = long_size(city)+1, state_s = long_size(state)+1, size = long_size(cat)+1;
	business* p = malloc(sizeof(struct business));
	p->business_cat=malloc(sizeof(char)*size);
	p->business_state=malloc(sizeof(char)*state_s);
	p->business_city=malloc(sizeof(char)*city_s);
	p->business_id=malloc(sizeof(char)*id_s);
	p->business_name=malloc(sizeof(char)*name_s);
	p->next=NULL;
	p->m_stars = stars;
	p->n_reviews = n_rev;
	strcpy(p->business_name,name);
	strcpy(p->business_id,id);
	strcpy(p->business_city,city);
	strcpy(p->business_state,state);
	strcpy(p->business_cat,cat);
	return p;
}

city_arr* init_city(){
	city_arr* c = malloc(sizeof(city_arr));
	c->city=NULL;
	c->top=NULL;
	c->last=NULL;
	c->next=NULL;
	return c;
}

void print_ct_arr(city_arr* c){
	top_arr* t; 
	while(c != NULL){
		printf("City: %s Business:",c->city);
		t=c->top;
		for(;t!=NULL;t=t->next)
			printf(" %s,",t->b_city->business_name);
		printf("\n");
		c=c->next;
	}
	printf("END\n");
}

void insert_city(city_arr* c, char* city){
	char* lower = calloc(1,long_size(city)+1);
	strcpy(lower,toLow(city));
	if(c->city==NULL){
		c->city=malloc(long_size(city)+1);
		strcpy(c->city,lower);
	}else{
		while(c->next!=NULL){
			if(strcmp(c->city,lower))
				c=c->next;
			else{
				return;
			}
		}
		if(strcmp(c->city, lower)){
			city_arr* c2 = init_city();
			c2->city=malloc(long_size(city)+1);
			strcpy(c2->city,toLow(city));
			c->next=c2;
		}
	}
    free(lower);
}

void sortedInsert(struct top_arr** head_ref,struct top_arr* new_node){
    struct top_arr* current;
    if (*head_ref == NULL || (*head_ref)->b_city->m_stars < new_node->b_city->m_stars) {
        new_node->next = *head_ref;
        *head_ref = new_node;
    }
    else {
        current = *head_ref;
        while (current->next != NULL && current->next->b_city->m_stars > new_node->b_city->m_stars) {
            current = current->next;
        }
        new_node->next = current->next;
        current->next = new_node;
    }
}

void insert_buz(city_arr* c, business* b){
    struct top_arr* new_node = NULL;
    for(;c != NULL;c = c->next){    
                if(!strcmp(c->city,toLow(b->business_city)))    
                        break;
    }
    new_node = (struct top_arr *)malloc(sizeof(struct top_arr));
    if (new_node == NULL){
        printf("Failed to insert element. Out of memory");
        return;
    }
    new_node->b_city = malloc(sizeof(business));
    new_node->b_city = b;
    new_node->next = NULL;

    if( c->top == NULL)
    {
        c->top = new_node;
	c->last = new_node;
    }
    else
	    sortedInsert(&c->top, new_node);
}

city_arr* per_city(SGR d){
	city_arr *c = init_city();
	business* b;
	for(int i = 0;i<MAX_TABLE;i++){
		b = d->business->hash_table_business[i]->head;
		while(b!=NULL){
			insert_city(c, b->business_city);
			insert_buz(c,b);
			b= b->next;
		}
	}
	return c;
}

int hashingB(char* state){
    int result;
    if (state[0]>state[1]){ //in case its in alphabetical order
        result = state[0]+state[1]+1; 
    } else { // cc
        result = state[0]+state[1];
    }
    return result;
}

void insert_cat(city_arr* c, business* b){
    struct top_arr* new_node = NULL;

    new_node = (struct top_arr *)malloc(sizeof(struct top_arr));
    if (new_node == NULL){
        printf("Failed to insert element. Out of memory");
        return;
    }
    new_node->b_city = malloc(sizeof(business));
    new_node->b_city = b;
    new_node->next = NULL;

    if( c->top == NULL)
    {
        c->top = new_node;
	c->last = new_node;
        return;
    }else
	    sortedInsert(&c->top, new_node);
}

city_arr* per_cat(SGR d,char* category){
	city_arr *c = init_city();
	business* b;
	for(int i = 0;i<MAX_TABLE;i++){
		b = d->business->hash_table_business[i]->head;
		while(b!=NULL){
			if(find_word_in_string(category, b->business_cat)){
			//	printf("%s\n",b->business_name);
				insert_cat(c,b);}
			b=b->next;
		}
	}
	return c;
}



