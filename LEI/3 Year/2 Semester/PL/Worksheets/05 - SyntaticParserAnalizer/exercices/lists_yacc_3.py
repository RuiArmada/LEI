import ply.yacc as yacc

from lists_lex

def p_inv_func(p):
    "inv_func : ID list"
    print('Words                  | ', p.parser.word)
    print('Numbers                | ', p.parser.num)
    print('Total Sum of Numbers   | ', p.parser.sum)
    print('List of Words          | ', p.parser.list_word)
    print('List                   | ', p.parser.list)

def p_list(p):
    "list : PE inside PD"
    pass

def p_inside_empty(p):
    "inside : "
    pass

def p_inside_elements(p):
    "inside : elements"
    pass

def p_elements_virg(p):
    "elements : element"
    pass

def p_elements_element(p):
    "elements : elements VIRG element "
    pass

def p_element_ID(p):
    "element : ID"
    parser.word += 1
    parser.list_word.append(1)
    pass

def p_element_NUM(p):
    "element : NUM"
    parser.num += 1
    val = p[1]
    p.parser.sum += val
    p.parser.list.append([1])
    pass

def p_error(p):
    print('Syntax ERROR')

parser = yacc.yacc()
parser.word = 0
parser.num = 0
parser.sum = 0
parser.list_word = []
parser.list = []

import sys

for line in sys.stdin:
    result = parser.parse(line)
    print('Result of Parsing  | ', result)
    parser.word = 0
    parser.num = 0
    parser.sum = 0
    parser.list_word = []
    parser.list = []
