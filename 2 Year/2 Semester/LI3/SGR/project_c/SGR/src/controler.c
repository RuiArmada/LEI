#include "../include/macro.h"
#include "../include/menu.h"
#include <stdio.h>
#include <termios.h>

static struct termios old, current;

/* Initialize new terminal i/o settings */
void initTermios(int echo) 
{
  tcgetattr(0, &old); /* grab old terminal i/o settings */
  current = old; /* make new settings same as old settings */
  current.c_lflag &= ~ICANON; /* disable buffered i/o */
  if (echo) {
      current.c_lflag |= ECHO; /* set echo mode */
  } else {
      current.c_lflag &= ~ECHO; /* set no echo mode */
  }
  tcsetattr(0, TCSANOW, &current); /* use these new terminal i/o settings now */
}

/* Restore old terminal i/o settings */
void resetTermios(void) 
{
  tcsetattr(0, TCSANOW, &old);
}

/* Read 1 character - echo defines echo mode */
char getch_(int echo) 
{
  char ch;
  initTermios(echo);
  ch = getchar();
  resetTermios();
  return ch;
}

/* Read 1 character without echo */
char getch(void) 
{
  return getch_(0);
}

/* Read 1 character with echo */
char getche(void) 
{
  return getch_(1);
}


void control() {
    //printf(HIDE_CURSOR);
    char buff; //letter; 
    int out = 0, row, column;
    //clock_t start, end;

    printMenu(0 , 0); 
    while(!out) {
	    buff = '\0';
	    row = 0;
	    column = 0;
	    while(buff != '\n') {
		    buff = getche();
		    if(buff == 'D' && row == LINE_1 && column == COLUMN_1)
			    column -= 1;
		    if(buff == 'C' && row == LINE_1 && column == COLUMN_2)
			    column +=1;
		    if(buff == 'A' && row == LINE_2)
			    row -=1;
		    if(buff == 'B' && row == LINE_1)
			    row  +=1;
		    if(!(row >=0 && row<=5)){
			    if(row>5)
				    row = 5;
			    else if (row<0)
				    row=0;
		    }
		    if(!(column >= 0 && column <= 2)){
			    if(column>3)
				    column = 2;
			    else if (column < 0)
				    column = 0;
		    }
	    }
	    printMenu(row , column); 
    }
}

/*
void main_loop(){
	control();
	char command[127];
	do{
		
	}while(find_word_in_string("quit", command))

}*/
