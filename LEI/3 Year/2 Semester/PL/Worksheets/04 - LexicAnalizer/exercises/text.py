import ply.lex as lex

import sys



f = open('../files/input.txt')



tokens = [
    'OPEN',
    'CLOSE',
    'TEXT'
    ]



states = [
    ('comment','exclusive')
]



def t_ANY_OPEN(t):
    r'/\*'
    t.lexer.push_state('comment')



def t_TEXT(t):
    r'.|\n'
    print(t.value,end='')



def t_comment_CLOSE(t):
    r'\*/'
    t.lexer.pop_state()



def t_comment_TEXT(t):
    r'.|\n'
    pass



def t_ANY_error(t):
    print('Illegal characters:' + t.value[0])
    t.lexer.skip(1)



lexer = lex.lex()
for line in sys.stdin:
    lexer.input(line)
    for tok in lexer:
        pass