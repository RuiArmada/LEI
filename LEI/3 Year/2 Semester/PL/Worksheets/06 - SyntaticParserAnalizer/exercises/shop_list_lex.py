import ply.lex as lex

tokens = (
    'ID',
    'INT', 
    'FLOAT', 
    'SEP'
    )

literals = ['-', ',', ':', ';']

t_SEP = r'::'
t_ID = r'\w+'


def t_FLOAT(t):
    r'\d+.\d+'
    t.value = float(t.value)
    return t


def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t


t_ignore = ' \n\t'


def t_error(t):
    print(f'Illegal character: {t.value[0]}')
    t.lexer.skip(1)


lexer = lex.lex()