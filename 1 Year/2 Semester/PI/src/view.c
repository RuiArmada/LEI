#include "../includes/view.h"

void printmenu () {
    puts("Type the number of the function to execute, 0 to exit:");
}

void menu (void_fn * func) {
    int choice;
    printmenu();
    scanf("%d", &choice);

    while (choice) {
            if (choice <= 0 || choice > 50)
                puts("Wrong input");
            else {
                if (!func[choice-1]())
                    puts("Working Function\n");
                else puts("Not finished Function\n");
            }
            printmenu();
            scanf("%d", &choice);
     }
}