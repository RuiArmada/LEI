// Author: Rui Filipe Pimenta Armada
// Date: 2021-15-02

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <limits.h>
#include <math.h>
#include <string.h>
#include "load.h"

void array(uint8_t* file, int current,int end, uint8_t* source){
	for(int i=0;i<DIC;i++)
		source[i]=0;
	for(int i = current;i<=end;i++)
		source[i-current]=file[i];
}

int comp_arrays(uint8_t* arr1,uint8_t* arr2){
	int r = 1;
	if (arr1[0]==0||arr2[0]==0)
		r=0;
	else
		for(int i=0;i<64;i++)
			if(arr1[i]!=arr2[i])
				r = 0;
	return r;
}

void convert(uint16_t help, uint8_t* out){
	int r=0,i=0;
	while(help>0){
		r = help%10;
		help = floor(help/10);
		out[i]=48 + (r*10);
		i++;
	}
}


void copy_arr(uint8_t* from, uint8_t* to){
        for(int i=0;from[i];i++)
                to[i]=from[i];
}

void write_dic(uint8_t* from,uint8_t* plus, uint8_t* to){
	int i = 0;
	for(i;plus[i];i++)
		to[i]=plus[i];
	for(int x = 0;from[x];x++){
		to[i]=from[x];
		i++;
	}
}

void algorithm(uint8_t* file,uint8_t** dic, uint16_t* out){
	int z = 1,x=2,h=0;
	uint8_t last[DIC]={0},*comp;
	comp = malloc(sizeof(uint8_t*) * DIC);
	int temp = DIC ,n_patterns=256,end=0;
	dic[n_patterns][0]=file[0];
	out[0]=file[0];
	dic[n_patterns][1]=file[1];
	out[1] = file[1];
	n_patterns++;
	for(int i = 2;i<BLOCK_SIZE && file[i]!=0;){
		h=i;
		temp=n_patterns-1;
		while(temp>255 && z){
			for(end=h;end<i+16 && z && strlen(comp)<2048;end++){
				array(file,i,end,comp);
				if(comp_arrays(comp,dic[temp])){
					out[x]=temp+1;
					z=0;
				}
			}
			temp--;
		}
		if(!z)
			i=end-1;
		if(z){
			out[x]=file[i];
			for(int o = 0;o<DIC;o++)
				comp[o]=0;
			comp[0]=file[i];
		}
		if (out[x-1]>256)
			copy_arr(dic[out[x-1]],last);
		else
			last[0]=out[x-1];
		if (strlen(comp) + strlen(last)>2048)
			goto skip;
		write_dic(comp,last, dic[n_patterns]);
		n_patterns++;
skip:
		for(int u = 0;u<DIC && last[u]!=0;u++)
			last[u]=0;
		z=1,x++;
		i++;
		printf("%d\n",i);

	}
}

// finds the length of a given file

int file_length(FILE *file) {
    int start, end;

    start = ftell(file);
    fseek(file, 0, SEEK_END);
    end = ftell(file);
    rewind(file);

    return (end - start);
}

int translate(uint16_t* in,char* out){
	uint16_t t = 0;
	char add[5]={0};
	for(int o = 0;o<BLOCK_SIZE+1;o++)
		out[o]=0;
	char temp[BLOCK_SIZE]={0};
	t = in[0]; 
        if (t>255){
		sprintf(temp, "%s(%d)", out, t);
        }    
        else{
		sprintf(add, "%c", t);
                sprintf(temp,"%s(\'%s\')", out, add); 
        }
        strcpy(out,temp);
	for(int i=1;i<BLOCK_SIZE && in[i]!=0;i++){
		t = in[i];
		if (t>255){
			sprintf(temp,"%s(%d)", out, t);
		}
		else{
			sprintf(add, "%c", t);
			sprintf(temp,"%s(\'%s\')", out, add);
		}
		strcpy(out,temp);
	}
	return strlen(out);
}



long double call_lzwd(char* file){
	char file_out[3064];
	long double size = 0, nBlocks =0;
	FILE *rfile, *wfile;
	uint8_t **dic;
	
	strcpy(file_out, file);
	strcat(file_out, ".lzwd");
	rfile= fopen(file,"rb");
	wfile= fopen(file_out,"w");

	size=file_length(rfile);
	nBlocks = ceil(size/BLOCK_SIZE);

	dic = calloc(DIC,sizeof(uint16_t*));
	for(int i = 0; i < DIC; ++i)
		dic[i] = calloc(BLOCK_SIZE ,sizeof(uint16_t));
        for(int i=0;i<DIC;i++){
                dic[i][0]=i;
        }
	for(;nBlocks>0;nBlocks--){
		uint16_t out[BLOCK_SIZE]={0};
		uint8_t test[BLOCK_SIZE]={0};
		fread(&test,1,BLOCK_SIZE,rfile);
		algorithm(test,dic,out);
		int write = translate(out,test);
		fwrite(&test,1,write-5,wfile);
	}
	free(dic);
	fclose(rfile);
	fclose(wfile);
	return nBlocks;
}

