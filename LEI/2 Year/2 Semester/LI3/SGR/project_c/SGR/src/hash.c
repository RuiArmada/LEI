
#include "../include/structs.h"
#include "../include/SGR.h"
#include "../include/load.h"
#include "../include/auxiliar.h"

size_t long_size(char* s){
	size_t i = 0;
	while(s[i++]!='\0');
	i+=1;
	return i;
}

size_t hash(const char *s){
	size_t hash, i,len = strlen(s);
	for(hash = i = 0; i < len; ++i){
		hash += s[i];
		hash += (hash << 10);
		hash ^= (hash >> 6);
	}
	hash += (hash << 3);
	hash ^= (hash >> 11);
    	hash += (hash << 15);
    	hash = hash%MAX_TABLE;
    	return hash;
}


user* new_u(char* name,char* id){
	size_t id_s = long_size(id)+1, name_s = long_size(name)+1;
	user* u = malloc(sizeof(struct user));
	u->user_name=malloc(sizeof(char)*name_s);
	u->user_id=malloc(sizeof(char)*id_s);
	strcpy(u->user_name, name);
	strcpy(u->user_id, id);
	u->next=NULL;
	u->n_reviews=0;
	return u;
}

business* new_b(char* name,char* id,char* city,char* state,char* cat){
	size_t name_s =long_size(name)+1, id_s = long_size(id)+1, city_s = long_size(city)+1, state_s = long_size(state)+1, size = long_size(cat)+1;
	business* p = malloc(sizeof(struct business));
	p->business_cat=malloc(sizeof(char)*size);
	p->business_state=malloc(sizeof(char)*state_s);
	p->business_city=malloc(sizeof(char)*city_s);
	p->business_id=malloc(sizeof(char)*id_s);
	p->business_name=malloc(sizeof(char)*name_s);
	p->next=NULL;
	p->m_stars = 0;
	p->n_reviews = 0;
	strcpy(p->business_name,name);
	strcpy(p->business_id,id);
	strcpy(p->business_city,city);
	strcpy(p->business_state,state);
	strcpy(p->business_cat,cat);
	return p;
}

reviews* new_r(char* id,char* b_id,char* u_id,float stars,char* text){
	size_t id_s = long_size(id)+1, b_s = long_size(b_id)+1, u_s = long_size(u_id)+1, size = long_size(text)+1;
	reviews* p = malloc(sizeof(struct reviews));
	p->next = NULL;
	p->text = malloc(sizeof(char)*size);
	p->user_id = malloc(sizeof(char)*u_s);
	p->review_id = malloc(sizeof(char)*id_s);
	p->business_id = malloc(sizeof(char)*b_s);
	strcpy(p->review_id,id);
	strcpy(p->user_id,u_id);
	strcpy(p->business_id,b_id);
	strcpy(p->text,text);
	p->stars = stars;
	return p;
}

void insert_hash_user(SGR d,user* u){
	size_t pos = hash(u->user_id);
	user* head = d->user->hash_table_user[pos]->head;
	if(head == NULL){
		d->user->hash_table_user[pos]->user = u;
		d->user->hash_table_user[pos]->head = u;
		d->user->hash_table_user[pos]->last = u;
	}
	else{
		user* last = d->user->hash_table_user[pos]->last;
		last->next=malloc(sizeof(user));
		last->next=u;
		u->next=NULL;
		d->user->hash_table_user[pos]->last = u;
	}
	d->user->n_users += 1;
}

void insert_hash_business(SGR d,business* b){
	size_t pos = hash(b->business_id);
	business* head = d->business->hash_table_business[pos]->head;
	if(head == NULL){
		d->business->hash_table_business[pos]->business = b;
		d->business->hash_table_business[pos]->head = b;
		d->business->hash_table_business[pos]->last = b;
	}
	else{
		business* last = d->business->hash_table_business[pos]->last;
		last->next = malloc(sizeof(business));
		last->next=b;
		b->next=NULL;
		d->business->hash_table_business[pos]->last = b;
	}
	d->business->n_business += 1;
}

int insert_hash_review(SGR d,reviews* r){
	size_t pos = hash(r->user_id), pos_b=hash(r->business_id);
	reviews* head = d->reviews->hash_table_review[pos]->head;
	user* u_tmp = d->user->hash_table_user[pos]->head;
	float stars = 0;
	while(u_tmp != NULL && strcmp(u_tmp->user_id,r->user_id))
		u_tmp = u_tmp->next;
	if(u_tmp != NULL){
		u_tmp->n_reviews +=1;
		if(head==NULL){
			d->reviews->hash_table_review[pos]->reviews = r;
			d->reviews->hash_table_review[pos]->head = r;
			d->reviews->hash_table_review[pos]->last = r;
		}
		else{
			reviews* last = d->reviews->hash_table_review[pos]->last;
			last->next = malloc(sizeof(reviews));
			last->next=r;
			r->next = NULL;
			d->reviews->hash_table_review[pos]->last = r;
		}
	}
	else
		return 0;
	business* b_tmp = d->business->hash_table_business[pos_b]->head;
	while(b_tmp !=NULL && strcmp(b_tmp->business_id,r->business_id))
		b_tmp = b_tmp->next;
	if (b_tmp != NULL){
		b_tmp->n_reviews += 1;
		stars = b_tmp->m_stars;
		stars += r->stars;
		stars = stars / 2;
		b_tmp->m_stars = stars;
	}
	d->reviews->n_reviews += 1;
	return 1;
}
