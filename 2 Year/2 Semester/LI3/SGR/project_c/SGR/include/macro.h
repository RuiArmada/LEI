/**    
 * @file macro.h    
 * @author Miguel Gomes, Ariana Lousada, Rui Armada.   
 * @date 26 Abril 2021
 * @brief Defines and global constants.   
 */ 
#ifndef __MACRO_H__
#define __MACRO_H__


/* ANSI Color Codes */
#define RED "\x1b[1;31m"
#define GREEN "\x1b[1;32m"  
#define YELLOW "\x1b[1;33m"
#define BLUE "\x1b[0;36m"
#define RESET_COLOR "\x1b[0m"
#define FLICKER "\033[5m"
#define RESET "\033[0m"

/* Cursor View Options */
#define HIDE_CURSOR "\x1B[?25l"
#define SHOW_CURSOR "\x1B[?25h"

/* Constant Definition */
#define TAM 5000
#define MAX 5000
#define SIZE_ID 24
#define MAX_ID 23
#define MAX_NAME 30
#define MAX_TABLE 200000
#define DICTIONARY 37 
#define TEXT_CHAR 5000
#define CATEGORIES 128
#define BUF 6000

/* Enter Key Definition */
#define ENTER 13

/* Arrow Keys Definition */
#define ARROW_UP 65
#define ARROW_DOWN 66
#define ARROW_RIGHT 67
#define ARROW_LEFT 68

/* Line constant definition */
#define LINE_1 0
#define LINE_2 1

/* Column constant definition */
#define COLUMN_1 0
#define COLUMN_2 1
#define COLUMN_3 2
#define COLUMN_4 3
#define COLUMN_5 4

#endif
