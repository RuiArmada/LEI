import ply.lex as lex
import sys

tokens = [
    'INT',
    'FLOAT',
    'ID'
]

literals = ['+', '-', '*', '/']

t_ID = r'\w+'

def t_FLOAT(t): 
    r'\d+\.\d{1,2}'
    t.value = float(t.value)
    return t

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

t_ignore = ' \t\n'

def t_error(t):
    print('Character ERROR ' + t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

for line in sys.stdin:
    lexer.input(line)
    for token in lexer:
        print(token)
