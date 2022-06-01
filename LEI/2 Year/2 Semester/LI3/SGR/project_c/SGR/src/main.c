#include "../include/controler.h"
#include "../include/load.h"
#include "../include/interpreter.h"
#define rows 24

int main(){
	char* use_f = "../Database/users_full.csv";
	char* bus_f = "../Database/business_full.csv";
	char* rev_f = "../Database/reviews_1M.csv";
	loop(use_f,bus_f,rev_f);
	return 1;
}
