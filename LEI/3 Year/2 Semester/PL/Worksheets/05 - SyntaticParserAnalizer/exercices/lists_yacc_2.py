import ply.yacc as yacc

from lists_lex import tokens

def p_inv_func(p):
    "inv_func : ID list"
    print('Parsing Completed!')
    p[0] = p[2]

def p_list(p):
    "list : PE inside PD"
    p[0] = p[2]

def p_inside_empty(p):
    "inside : "
    p[0] = []

def p_inside_elements(p):
    "inside : elements"
    p[0] = p[1]

def p_elements_element(p):
    "elements : element"
    p[0] = [p[1]]

def p_elements_virg(p):
    "elements : elements VIRG element"
    p[0] = [p[1]] + p[3]

def p_element_ID(p):
    "element : ID"
    p[0] = p[1]

def p_element_NUM(p):
    "element : NUM"
    p[0] = p[1]

def p_error(p):
    print('Syntax ERROR')

parser = yacc.yacc()

import sys

for line in sys.stdin:
    result = parser.parse(line)
    print('Result of Parsing  | ', result)
