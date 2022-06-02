#include "../include/structs.h"
#include "../include/SGR.h"
#include "../include/load.h"
#include "../include/utils.h"
#include "../include/hash.h"
#include "../include/menu.h"

#include <ctype.h>
#include <stdarg.h>
#include <math.h>

#define rows 24
#define ID_P 10
#define BUS_P 5

TABLE2 **to_list2(TABLE2 * t,double size){
	int i = 0,cur = 0;
	size = ceil(size/BUS_P);
	TABLE2 **out = malloc(sizeof(TABLE2)*size);
	i=0;
	while(t){
		if(!(i%BUS_P)){
			out[cur] = malloc(sizeof(TABLE2));
			out[cur] = t;
			cur++;
		}
		i++;
		t = t->next;
	}
	return out;
}

void print_page_2(TABLE2* page){
	clrscr();
	for(int i=0;i<rows+2;i++)
		putc('-',stdout);
	putc('\n',stdout);
	for(int i = 0;i<5 && page!= NULL;i++){
		printf("| %s\n",page->info->business_id);
		printf("| %s\n",page->info->business_name);
		printf("| %s\n",page->info->business_city);
		printf("| %s\n",page->info->business_state);
		printf("| Reviews:%zu# Stars:%.2f\n",page->info->n_reviews,page->info->m_stars);
		printf("--------------------------\n");
		page = page->next;
	}
}

TABLEIDS **to_list(TABLEIDS * t,double size){
	int i = 0, cur = 0;
	size = ceil(size/ID_P);
	TABLEIDS **out = malloc(sizeof(TABLEIDS)*size);
	i=0;
	while(t){
		if(!(i%ID_P)){
			out[cur] = malloc(sizeof(TABLEIDS));
			out[cur] = t;
			cur++;
		}
		i++;
		t = t->next;
	}
	return out;
}

void print_page_id(TABLEIDS* page){
	clrscr();
	for(int i=0;i<rows+2;i++)
		putc('-',stdout);
	putc('\n',stdout);
	for(int i = 0;i<10 && page!= NULL;i++){
		printf("| %s |\n",page->id);
		printf("--------------------------\n");
		page = page->next;
	}
}

void pages(TABLE t){
	double size_i = 0,size_2 = 0;
	TABLEIDS * temp = t->ids;
	for(;temp;temp = temp->next,size_i++);
	if(t->ids->id){
		TABLEIDS **list_ti = to_list(t->ids,size_i);
		char buf[10];
		size_t cur = 0;
		while(cur<ceil(size_i/10)){
			print_page_id(list_ti[cur]);
			printf("Page %zu/%.0lf\n",cur,ceil(size_i/10)-1);
			printf("(next/previous/page_#/quit):");
			scanf("%s",buf);
			if(isdigit(buf[0])){
				cur = atoi(buf);
				if(cur>(ceil(size_i/10)))
					cur = ceil(size_i/10)-1;
				else cur = 0;
			}
			if (buf[0] == 'n')
				cur += 1;
			if(buf[0] == 'p')
				cur -= 1;
			if(buf[0] == 'q')
				cur = ceil(size_i/10);
		}

	}else if (t->table->info){
		TABLE2 * temp2 = t->table;
		for(;temp2;temp2 = temp2->next,size_2++);
		TABLE2 **list_t2 = to_list2(t->table,size_2);
		char buf[20];
		size_t cur = 0;
		while(cur<ceil(size_2/5)){
			print_page_2(list_t2[cur]);
			printf("Page %zu/%.0lf\n",cur,ceil(size_2/5)-1);
			printf("(next/previous/page_#/quit):");
			scanf("%s",buf);
			if(isdigit(buf[0])){
				cur = atoi(buf);
				if(cur>(ceil(size_2/5)))
					cur = ceil(size_2/5)-1;
				else cur = 0;
			}
			if (buf[0] == 'n')
				cur += 1;
			if(buf[0] == 'p')
				cur -= 1;
			if(buf[0] == 'q')
				cur = ceil(size_2/5);
		}
	}
}
