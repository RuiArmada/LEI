#include <unistd.h> 

#include "common/protocol.h"

void helper() {
    
    WRITE_LITERAL(1,"\n---->>>> Here's a list of the avalaible commands! <<<<----\n\n");

    
    WRITE_LITERAL(1," -i <argumento1>, inactive-time <argumento1>\n-> Defines maximum time, in seconds, of inactivity of comunication in an anonymus pipe\n\n");

    
    WRITE_LITERAL(1," -m <argumento1>, exec-time <argumento1>\n-> Defines maximum time (seconds) of execution in a task\n\n");

    
    WRITE_LITERAL(1," -e <p1|p2|...|pn>, execute <p1|..|pn>\n-> Executes a task in the command line\n\n");

    
    WRITE_LITERAL(1," -l, list\n-> Lists the tasks in execution\n\n");

    
    WRITE_LITERAL(1," -t n, terminate	n\n-> Terminates the task that is being executed\n\n");

    
    WRITE_LITERAL(1," -r, history\n-> History of terminated tasks\n\n");
}

