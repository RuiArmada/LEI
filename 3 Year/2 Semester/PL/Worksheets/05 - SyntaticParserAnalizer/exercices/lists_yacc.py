import ply.yacc as yacc

from lists_lex import tokens

def p_grammar(p):
    """
        inv_func : ID list

        list : PE inside PD

        inside :
               | elements

        elements : element
                 | element VIRG elements

        element : ID
                | NUM
    """
    pass

def p_error(p):
    print('Syntax ERROR')
    parser.success = False

parser = yacc.yacc()

import sys

for line in sys.stdin:
    parser.success = True
    parser.parse(line)
    if parser.success:
        print('Parsing Completed.')
        