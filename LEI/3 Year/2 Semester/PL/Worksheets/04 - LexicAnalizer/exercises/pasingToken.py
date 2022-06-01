import ply.lex as lex

tokens = [
    'ON',
    'OFF',
    'EQUAL',
    'NUMBER'
]

flag = False
sum = 0

t_ignore = ' \t\n'

def t_ON(t):
    r'[Oo][Nn]'
    global flag
    flag = True
    print('Sum Enabled')
    return t

def t_OFF(t):
    r'[Oo][Ff][Ff]'
    global flag
    flag = False
    print('Sum Disabled')
    return t

def t_EQUAL(t):
    r'='
    print('Actual Sum: ', sum)
    return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    if flag:
        global sum
        sum = sum + t.value
    return t

def t_error(t):
    print('Character ERROR ' + t.value)


lexer = lex.lex()

data = 'on 1 2 3=off 1 2 =on 1 1='

lexer.input(data)

for token in lexer:
    print(token)