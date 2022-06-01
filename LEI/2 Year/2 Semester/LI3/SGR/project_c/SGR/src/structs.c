#include "../include/structs.h"

TABLE emptyTable(){
    TABLE t = malloc(sizeof(TABLE));
	t->ids=malloc(sizeof(TABLEIDS));
	t->table = malloc(sizeof(TABLE2));
	t->table->info=malloc(sizeof(business));
	t->table->info = NULL;
	t->table->next = NULL;
	t->last=t->table;
    	t->ids->id = malloc(sizeof(char)*23);
	t->ids->id = NULL;
	t->ids->next = NULL;
	t->lastID = t->ids;
	return t;
}

void printTable(TABLE t){
    while(t->ids != NULL){
        printf("%s\n", t->ids->id);
        t->ids = t->ids->next;
        }
}

void insert_table(business* b, TABLE t){
    struct TABLE2* new_node = NULL;
    struct TABLE2* last = NULL;
    new_node = malloc(sizeof(struct TABLE2));
    if (new_node == NULL){
        printf("Failed to insert element. Out of memory");
        return;
    }
    new_node->info = malloc(sizeof(business));
    new_node->info = b;
    new_node->next = NULL;

    if( t->table->info == NULL)
    {
        t->table = new_node;
	t->last = new_node;
    }
    else{
	    last = t->last;
	    last->next = new_node;
	    t->last = new_node;
    }
}

void insert_id_table(char* r_id, TABLE t){
    TABLEIDS* t2 = t->ids;
    if(t2->id == NULL){
        t2->id = r_id;
        t2->next = NULL;
        t->ids = t2;
        t->lastID = t2;
    }
    
    else {
        t2 = t->lastID;
        t2->next = malloc(sizeof(TABLEIDS));
        t2->next->id = r_id;
        t2->next->next = NULL;
        t2 = t2->next;
	    t->lastID->next = t2;
        t->lastID = t2;
        
	}
}
