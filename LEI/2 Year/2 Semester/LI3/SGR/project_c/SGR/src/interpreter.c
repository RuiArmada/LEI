#include "../include/load.h"
#include "../include/utils.h"
#include "../include/pagination.h"
#include "../include/hash.h"
#include "../include/menu.h"
#include "../include/interpreter.h"

#include <stdio.h>
#include <time.h>
#include <stdlib.h>

char *COLUMNS[7] = {"business_id","business_name","business_city","business_state","business_stars","business_reviews","business_categories"};

char acOpen[]  = {"\"<{"};
char acClose[] = {"\">}"};

char* remove_quotes(char* s){
	char *r, *w;
	for (w = r = s; *r; r++) {
		if (*r != '"') {
			*w++ = *r;
		}
	}
	*w = '\0';
	return s;
}

char *strmbtok ( char *input, char *delimit, char *openblock, char *closeblock) {
    static char *token = NULL;
    char *lead = NULL;
    char *block = NULL;
    int iBlock = 0;
    int iBlockIndex = 0;

    if ( input != NULL) {
        token = input;
        lead = input;
    }
    else {
        lead = token;
        if ( *token == '\0') {
            lead = NULL;
        }
    }

    while ( *token != '\0') {
        if ( iBlock) {
            if ( closeblock[iBlockIndex] == *token) {
                iBlock = 0;
            }
            token++;
            continue;
        }
        if ( ( block = strchr ( openblock, *token)) != NULL) {
            iBlock = 1;
            iBlockIndex = block - openblock;
            token++;
            continue;
        }
        if ( strchr ( delimit, *token) != NULL) {
            *token = '\0';
            token++;
            break;
        }
        token++;
    }
    return lead;
}

char** split_at(char* string,char* del){
	char ** value = (char **)calloc(1,10 * sizeof(char*));
	for(int i = 0; i < 10; i++) value[i] = (char *)calloc(1,100 * sizeof(char)+1);
	char* tok;
	int i = 0;
	tok = strmbtok ( string, del, acOpen, acClose);    
	strcpy(value[i],tok);    
	i++;    
	while ( ( tok = strmbtok ( NULL, del, acOpen, acClose)) != NULL){
		strcpy(value[i],tok);
		i++;
	}
	return value;
}

int get_col(char* value){
	for(int i = 0;i<7;i++){
		if(find_word_in_string(value, COLUMNS[i]))
			return i;
	}
	return 0;
}

/*
int comp_bus(business* b, char* col, char* val, enum OPERATOR i){
	int col_i = get_col(col);

}
TABLE compare_operator(TABLE t, char* col, char* val,enum OPERATOR i){
	TABLE r = emptyTable();
	if(t->ids){
		for(;t->ids;t->ids=t->ids->next){
			if(comp_ids(t->ids->id,val,i)){
				addRevInTABLE(t->ids->id,r);
			}
		}

	}
	else if (t->table){
		for(;t->table;t->table=t->table->next){
			if(comp_bus(t->table->info,col,val,i)){
				insertBinTABLE(r,t->table->info)
			}
		}

	}
	return r;
}
*/

void freeList_ids(struct TABLEIDS* head)
{
   struct TABLEIDS* tmp;

   while (head != NULL)
    {
       tmp = head;
       head = head->next;
       free(tmp);
    }

}

void freeList_2(struct TABLE2* head)
{
   struct TABLE2* tmp;

   while (head != NULL)
    {
       tmp = head;
       head = head->next;
       free(tmp);
    }

}
void free_table(TABLE t){
	if(t->ids->id!=NULL){
		freeList_ids(t->ids);
	}
	if(t->table->info!=NULL){
		freeList_2(t->table);
	}
}

void reverse(char s[])
 {
     int i, j;
     char c;

     for (i = 0, j = strlen(s)-1; i<j; i++, j--) {
         c = s[i];
         s[i] = s[j];
         s[j] = c;
     }
}  

void itoa(int n, char s[])
 {
     int i, sign;

     if ((sign = n) < 0)  /* record sign */
         n = -n;          /* make n positive */
     i = 0;
     do {       /* generate digits in reverse order */
         s[i++] = n % 10 + '0';   /* get next digit */
     } while ((n /= 10) > 0);     /* delete it */
     if (sign < 0)
         s[i++] = '-';
     s[i] = '\0';
     reverse(s);
}

void WriteFrmtd(FILE *stream, char *format, ...) {
   va_list args;

   va_start(args, format);
   vfprintf(stream, format, args);
   va_end(args);
}

void show(TABLE t){
	pages(t);
	free_table(t);
}

TABLE fromCSV(char* path,char del){
	char *name = calloc(1, 30), *id = calloc(1, 23), *city = calloc(1, 23), *state = calloc(1, 23), *categories = calloc(1,128);
	float stars = 0;
	size_t n_rev = 0;
	int col=0;
	FILE *fp = fopen(path,"r");
	if(!fp)
		return NULL;
	TABLE t = emptyTable();
	char *r_line = calloc(1,3000);
	while(fgets(r_line, 3000, fp) != NULL){
		if(charcount(r_line) < 23){
			if(sscanf(r_line,"%22[^\n]",id) == 1){
				printf("%s\n",id);
				insert_id_table(id, t);
			}
		}
		else{
			char* value = strtok(r_line, &del);
			while(value){
				if(col == 0){
					strcpy(id,value);
				}
				if(col == 1){
					strcpy(name,value);
				}
				if(col == 2){
					strcpy(city,value);
				}
				if(col == 3){
					strcpy(state,value);
				}
				if(col == 4){
					n_rev = atoi(value);
				}
				if(col == 5){
					stars = atof(value);
				}
				if(col == 6){
					strcpy(categories,value);
				}
				value = strtok(NULL, ", ");
				col++;
			}
			insert_table(new_b_l(name,id,city,state,n_rev,stars,categories),t);
		}
	}
	fclose(fp);
	return t;
}

TABLE get_pos(TABLE t,int x, int y){
	int iy = 0;
	char num_to_int[MAX_ID];
	TABLE out = emptyTable();
	if(t->ids->id != NULL){
		TABLEIDS *real_id = t->ids;
		for(;iy<y;iy++,real_id = real_id->next);
		insert_id_table(real_id->id,out);
	}
	else if(t->table->info != NULL){
		TABLE2 *real_t = t->table;
		for(;iy<y;iy++,real_t = real_t->next)
			switch(x){
				case 0:
					insert_id_table(real_t->info->business_id,out);
					__attribute__((fallthrough));
				case 1:
					insert_id_table(real_t->info->business_name,out);
					__attribute__((fallthrough));
				case 2:
					insert_id_table(real_t->info->business_city,out);
					__attribute__((fallthrough));
				case 3:
					insert_id_table(real_t->info->business_state,out);
					__attribute__((fallthrough));
				case 4:
					itoa(real_t->info->n_reviews,num_to_int);
					insert_id_table(num_to_int,out);
					__attribute__((fallthrough));
				case 5:
					itoa(real_t->info->m_stars,num_to_int);
					insert_id_table(num_to_int,out);
					__attribute__((fallthrough));
				case 6:
					insert_id_table(real_t->info->business_cat,out);
					__attribute__((fallthrough));
				default:
					break;
			}
	}
	return out;
}

void toCSV(TABLE t,char del,char* path){
	FILE *fp = fopen(path,"w");
	if(!fp && !t)
		return;
	if(t->ids)
		for(;t->ids;t->ids=t->ids->next)
			WriteFrmtd(fp, "%s\n",t->ids->id);
	else
		if(t->table){
			for(;t->table;t->table=t->table->next)
				WriteFrmtd(fp,"%s%c%s%c%s%c%s%c%zu%c%f%c%s\0",
						t->table->info->business_id,
						del,
						t->table->info->business_name,
						del,
						t->table->info->business_city,
						del,
						t->table->info->business_state,
						del,
						t->table->info->n_reviews,
						del,
						t->table->info->m_stars,
						del,
						t->table->info->business_cat);
		}

	fclose(fp);
}

void print_help(){
	printMenu(10, 10);
}

void prompt(double time){
	printf("The last action took: %lf milisseconds\n↳",time);
	puts("Insert command.");
	putc('>',stdout);
}


void loop(char* u,char* b, char* r){
	char command[127];
	double time = 0;
	clock_t Ticks[2];
	Ticks[0] = clock();
	SGR d = init_sgr();
	Ticks[1] = clock();
	time = (Ticks[1] - Ticks[0]) * 1000.0 / CLOCKS_PER_SEC;
	logo_Frame();
	do{
		prompt(time);
		scanf("%s",command);
		char** comands = split_at(command,"([, )");
		
		if(!(strcmp(comands[0],"show"))){
			if(!(strcmp(comands[1],"businesses_started_by_letter"))){
				Ticks[0] = clock();
				TABLE t = businesses_started_by_letter(d,comands[3][1]);
				Ticks[1] = clock();
				show(t);
				time = (Ticks[1] - Ticks[0]) * 1000.0 / CLOCKS_PER_SEC;

			}else if(!(strcmp(comands[1],"business_info"))){
				Ticks[0] = clock();
				TABLE t = business_info(d,remove_quotes(comands[3]));
				Ticks[1] = clock();
				show(t);
				time = (Ticks[1] - Ticks[0]) * 1000.0 / CLOCKS_PER_SEC;
				
			}else if(!(strcmp(comands[1],"businesses_reviewed"))){
				Ticks[0] = clock();
				TABLE t = businesses_reviewed(d,remove_quotes(comands[3]));
				Ticks[1] = clock();
				show(t);
				time = (Ticks[1] - Ticks[0]) * 1000.0 / CLOCKS_PER_SEC;

			}else if(!(strcmp(comands[1],"businesses_with_stars_and_city"))){
				Ticks[0] = clock();
				TABLE t = businesses_with_stars_and_city(d,atof(comands[3]),remove_quotes(comands[4]));
				Ticks[1] = clock();
				show(t);
				time = (Ticks[1] - Ticks[0]) * 1000.0 / CLOCKS_PER_SEC;

			}else if(!(strcmp(comands[1],"top_businesses_by_city"))){
				Ticks[0] = clock();
				TABLE t = top_businesses_by_city(d,atoi(comands[3]));
				Ticks[1] = clock();
				show(t);
				time = (Ticks[1] - Ticks[0]) * 1000.0 / CLOCKS_PER_SEC;

			}else if(!(strcmp(comands[1],"international_users"))){
				Ticks[0] = clock();
				TABLE t = international_users(d);
				Ticks[1] = clock();
				show(t);
				time = (Ticks[1] - Ticks[0]) * 1000.0 / CLOCKS_PER_SEC;

			}else if(!(strcmp(comands[1],"top_businesses_with_category"))){
				Ticks[0] = clock();
				TABLE t = top_businesses_with_category(d,atoi(comands[3]),remove_quotes(comands[4]));
				Ticks[1] = clock();
				show(t);
				time = (Ticks[1] - Ticks[0]) * 1000.0 / CLOCKS_PER_SEC;

			}else if(!(strcmp(comands[1],"reviews_with_word"))){
				Ticks[0] = clock();
				TABLE t = reviews_with_word(d,remove_quotes(comands[3]));
				Ticks[1] = clock();
				show(t);
				time = (Ticks[1] - Ticks[0]) * 1000.0 / CLOCKS_PER_SEC;

			}else continue;
		}
		if(!(strcmp(command, "help"))){
			Ticks[0] = clock();
			print_help();
			Ticks[1] = clock();
			time = (Ticks[1] - Ticks[0]) * 1000.0 / CLOCKS_PER_SEC;
		}
		if(!(strcmp(comands[0], "load_sgr"))){
			Ticks[0] = clock();
			if(find_word_in_string("", comands[1])){
				d = load_sgr(u,b,r);}
			else{
				d = load_sgr(remove_quotes(comands[1]),remove_quotes(comands[2]),remove_quotes(comands[3]));
				}
			Ticks[1] = clock();
			time = (Ticks[1] - Ticks[0]) * 1000.0 / CLOCKS_PER_SEC;
		}


		
	}while(!(find_word_in_string("quit", command)));
	free_sgr(d);
}


