import sys

import ply.yacc as yacc

from shop_list_yacc import tokens


def p_lisp_grammar(p):
    """
    list : categories

    categories : categories category
    categories :

    category : ID ':' products
    
    products : product
    products : products product

    product : '-' code SEP name SEP price SEP quantity ';'

    code  :  INT

    name :  ID

    price :  FLOAT

    quantity    :  INT
    """


def p_error(p):
    print('Syntax error!')


parser = yacc.yacc()


content = ""
for line in sys.stdin:
    content += line
parser.parse(content)
