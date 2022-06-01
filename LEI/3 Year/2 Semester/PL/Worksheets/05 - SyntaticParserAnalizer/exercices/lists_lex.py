import ply.lex as lex

tokens = (
    'ID',
    'NUM',
    'PE',
    'PD',
    'VIRG'
)

t_ID = r'\w+'
t_PE = r'\('
t_PD = r'\)'
t_VIRG = r','


def t_NUM(t):
    r'\d+'
    t.value = int(t.value)
    return t

t_ignore = '\n\t'

def t_error(t):
    print(f"Illegal character '{t.value[0]}'")

lexer = lex.lex()

