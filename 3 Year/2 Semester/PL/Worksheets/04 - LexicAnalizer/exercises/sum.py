import ply.lex as lex
import sys

tokens = [
    'ON',
    'OFF',
    'EQUAL',
    'NUMBER'
]

states = [
    ('sum','exclusive')
]

sum = 0

def t_NUMBER(t):
    r'\d+'
    pass

def t_ANY_EQUAL(t):
    r'='
    print('Actual Sum: ' + t.lexer.sum)

def t_ON(t):
    r'(?i:on)'
    t.lexer.begin('sum')
    print('Sum is Enabled.')

def t_sum_NUMBER(t):
    r'\d+'
    t.lexer.sum += int(t.value)

def t_sum_OFF(t):
    r'(?i:off)'
    t.lexer.begin('INITIAL')
    print('Sum is Disabled.')

t_ANY_ignore = '\n\t'

def t_ANY_error(t):
    print('Illegal Character: ', t.value[0])
    t.lexer.skip(1)

def t_INITIAL_sum_EQUAL(t):
    r'='
    print('Actual Sum: ', t.lexer.sum)

t_INITIAL_sum_ignore = '\n\t'

def t_INITIAL_sum_error(t):
    print('Illegal Character at "sum" state: ', t.value(0))
    t.lexer.skip(1)

lexer = lex.lex()

#State
lexer.sum = 0

for line in sys.stdin:
    lexer.input(line)
    for tok in lexer:
        pass