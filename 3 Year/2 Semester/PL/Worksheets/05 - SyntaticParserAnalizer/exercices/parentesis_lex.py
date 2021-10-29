# ()
# ()()
# (())(()())(())

import ply.lex as lex

tokens = [
    'PE',
    'PD'
]

t_PE = r'\('
t_PD = r'\)'

t_ignore = ' \n\t'

def t_error(t):
    print('Illegal Character: ' + t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()
