/**    
 * @file menu.h    
 * @author Miguel Gomes, Ariana Lousada, Rui Armada.   
 * @date 26 Abril 2021
 * @brief Menu methods.   
 */ 
#ifndef __MENU_H__
#define __MENU_H__


/**
 * @brief Clear the Screan
*/
void clrscr();

void logo_Frame();

/**
 * @brief It will print the menu off the app to the screen
 * @param row number of the selected row
 * @param column number of the selected column
*/
void printMenu(int row, int column);

#endif
