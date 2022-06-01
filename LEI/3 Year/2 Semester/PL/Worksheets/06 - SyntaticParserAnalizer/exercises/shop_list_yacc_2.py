import ply.yacc as yacc

from shop_list_lex import tokens

def p_list(p):
    "list : categories"
    print('Total Value: ', p[1])

def p_categories_category(p):
    "categories : categories category"
    p[0] = p[1] + p[2]

def p_categories_empty(p):
    "categories : "
    p[0] = 0

def p_category(p):
    "category : STR ':' products"
    p[0] = p[3]

def p_products_product(p):
    "products : product"
    p[0] = p[1]

def p_products_products(p):
    "products : products product"
    p[0] = p[1] + p[2]

def p_product(p):
    "product : '-' INT SEP STR SEP FLOAT SEP INT ';'"
    p[0] = p[6] * p[8]

def p_error(p):
    print('Syntax ERROR')

parser = yacc.yacc()

import sys

for line in sys.stdin:
    parser.parse(content)