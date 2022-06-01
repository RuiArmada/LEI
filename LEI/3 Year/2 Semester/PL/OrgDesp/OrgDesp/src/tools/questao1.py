import re

def questao1(data,equipa,terra):
    re_equipa = r'(?i)(' + '|'.join(equipa) + '|"")'
    re_terra = r'(?i)(' + '|'.join(terra) + '|"")'

    print("Concorrentes inscritos:" + ','.join(equipa) +
          " de " + ','.join(terra) + " :")
    for element in data:  
        if (re.search(re_equipa, element[6]) != None) and (
            re.search(re_terra, element[2]) != None):
                print("\t" + str(element[0]).upper())