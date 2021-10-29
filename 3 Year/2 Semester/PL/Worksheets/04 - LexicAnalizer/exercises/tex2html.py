import ply.lex as lex
import sys





tokens = [
    'BOLD',
    'ITALIC',
    'UNDERLINE',
    'SECTION',
    'SUBSECTION',
    'SUBSUBSECTION',
    'BENUMERATE',
    'EENUMERATE',
    'BITEMIZE',
    'EITEMIZE',
    'ITEM',
    'TEXT'
]





states = [
    ('bold', 'inclusive'),
    ('italic', 'inclusive'),
    ('underline', 'inclusive'),
    ('section', 'inclusive'),
    ('subsection', 'inclusive'),
    ('subsubsection', 'inclusive'),
    ('enumerate', 'inclusive'),
    ('itemize', 'inclusive'),
    ('item', 'inclusive'),
]





def t_BOLD(t):
    r'\\textbf\{'
    print('<b>', end='')
    t.lexer.push_state('bold')



def t_bold_end(t):
    r'\}'
    print('</b>', end='')
    t.lexer.pop_state()





def t_ITALIC(t):
    r'\\textit\{'
    print('<i>', end='')
    t.lexer.push_state('italic')



def t_italic_end(t):
    r'\}'
    print('</i>', end='')
    t.lexer.pop_state()





def t_UNDERLINE(t):
    r'\\underline\{'
    print('<u>', end='')
    t.lexer.push_state('underline')



def t_underline_end(t):
    r'\}'
    print('</u>', end='')
    t.lexer.pop_state()





def t_SECTION(t):
    r'\\section\{'
    print('<h1>', end='')
    t.lexer.push_state('section')



def t_section_end(t):
    r'\}'
    print('</h1>', end='')
    t.lexer.pop_state()





def t_SUBSECTION(t):
    r'\\subsection\{'
    print('<h2>', end='')
    t.lexer.push_state('subsection')



def t_subsection_end(t):
    r'\}'
    print('</h2>', end='')
    t.lexer.pop_state()





def t_SUBSUBSECTION(t):
    r'\\subsubsection\{'
    print('<h3>', end='')
    t.lexer.push_state('subsubsection')



def t_subsubsection_end(t):
    r'\}'
    print('</h3>', end='')
    t.lexer.pop_state()





def t_BENUMERATE(t):
    r'\\begin\{enumerate\}'
    print('<ol>', end='')
    t.lexer.push_state('enumerate')





def t_enumerate_EENUMERATE(t):
    r'\\end\{enumerate\}'
    print('</ol>', end='')
    t.lexer.pop_state()





def t_BITEMIZE(t):
    r'\\begin\{itemize\}'
    print('<ul>', end='')
    t.lexer.push_state('itemize')





def t_itemize_EITEMIZE(t):
    r'\\end\{itemize\}'
    print('</ul>', end='')
    t.lexer.pop_state()





def t_enumerate_itemize_ITEM(t):
    r'\\item\ +'
    print('<li>', end='')
    t.lexer.push_state('item')





def t_item_end(t):
    r'\n'
    print('</li>', end='')
    t.lexer.pop_state()





def t_TEXT(t):
    r'.|\n'
    print(t.value, end='')





def t_ANY_error(t):
    print('Illegal character:', t.value[0])
    t.lexer.skip(1)





lexer = lex.lex()
for line in sys.stdin:
    lexer.input(line)
    for tok in lexer:
        pass