import ply.yacc as yacc

from parentesis_lex import tokens

def p_grammar(p):
    """

    language : list_parentesis
            
    list_parentesis : parentesis list_parentesis
                    | 

    parentesis : PE list_parentesis PD

    """

def p_error(p):
    print('Syntax ERROR')

parser = yacc.yacc()

import sys

for line in sys.stdin:
    parser.parse(line) 