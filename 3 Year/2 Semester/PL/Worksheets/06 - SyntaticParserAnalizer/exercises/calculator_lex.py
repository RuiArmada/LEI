import ply.lex as lex

tokens = ['NUM','ID']
literals = ['+','-','*','/','(',')','?','!','=','$']

t_ID = r'\w+'

def t_NUM(t):
    r'\d+'
    t.value = int(t.value)
    return t

t_ignore = ' \n\t'

def t_error(t):
    print('Illegal caracter' + t.value[0])
    t.lexer.skip()

lexer = lex.lex()