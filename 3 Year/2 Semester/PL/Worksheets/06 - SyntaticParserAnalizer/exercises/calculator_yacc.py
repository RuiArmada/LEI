import ply.yacc as yacc

from calculator_lex import tokens

def p_calculator(p):
    "calculator : commands"

def p_commands(p):
    '''

    commands : commands command
             |

    '''

def p_read_command(p):
    "command : '?' ID"
    v = int(input('Insert a valid Value: '))
    p.parser.vars[p[2]] = v

def p_write_command(p):
    "command : '!' exp"
    print(p[2])

def p_atribute_command(p):
    "command : ID '=' exp"
    p.parser.vars[p[1]] = p[3]

def p_info_command(p):
    "command : '!' '!'"
    print(p.parser.vars)

def p_declaration_command(p):
    "command : '$' ID"
    p.parser.variaveis.update({p[2]: 0})

def p_exp_plus(p):
    "exp : exp '+' term"
    p[0] = p[1] + p[3]

def p_exp_minus(p):
    "exp : exp '-' term"
    p[0] = p[1] - p[3]

def p_exp_term(p):
    "exp : term"
    p[0] = p[1]

def p_term_mult(p):
    "term : term '*' factor"
    p[0] = p[1] * p[3]

def p_term_div(p):
    "term : term '/' factor"
    if p[3] == 0:
        print('ERROR - Division by 0...')
        p[0] = 0
    else:
        p[0] = int(p[1] / p[3])

def p_factor_id(p):
    "factor : ID"
    if p[1] not in p.parser.vars:
        print('NOTICE - Variable not Detected:  {p[1]}  Will initiate the variable at 0 value.')
        p.parser.vars[p[1]] = 0
    p[0] = p.parser.vars[p[1]]

def p_term(p):
    "term : factor"
    p[0] = p[1]

def p_factor_num(p):
    "factor : NUM"
    p[0] = p[1]

def p_factor_exp(p):
    "factor : '(' exp ')'"
    p[0] = p[2]



def p_error(p):
    print("Syntax error")

parser = yacc.yacc()
parser.vars = {}    

import sys

for line in sys.stdin:
    resultado = parser.parse(line)
