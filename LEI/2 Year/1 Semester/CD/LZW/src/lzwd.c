// Author: Rui Filipe Pimenta Armada
// Date: 2021-15-02

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <limits.h>
#include <math.h>
#include <string.h>
#include "load.h"
#include <time.h>

long double call_module(char *filename) {
    int r = call_lzwd(filename);
    if (!r)
      printf("Invalid Data\n");
    return r;
}

int main(int argc, char *argv[]) {
  long double var = 0;

  clock_t start = clock();

  int error = 0;
  if (argc > 2 || argc <= 1) {
    printf("Too many arguments\n");
    error = 1;
  }
  else {
    char *ficheiro = argv[1];
    int debug = 0;
    var = call_module(ficheiro); 
  } 
  clock_t end = clock();
	long double cpu_time_used = ((double) (end - start))*1000 / (CLOCKS_PER_SEC);

	printf("Rui Filipe Pimenta Armada,n A90468, MIEI/CD, 15-fev-2021\n");
	printf("Number of Blocks: %Le\n",var);   
	printf("Program runtime (millisseconds): %d\n",cpu_time_used);
	printf("Generated Files: %s.lzwd\n",argv[1]);

  return error;
}
