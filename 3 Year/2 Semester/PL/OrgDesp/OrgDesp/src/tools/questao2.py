import re

def questao2(data, nomes, emails):
    re_nomes = r'(?i)(' + '|'.join(nomes) + ')'
    re_email = r'(?i)(' + '|'.join(emails) + ')'

    for element in data:
        if (re.search(re_nomes, element[0]) != None) and (
            re.search(re_email,element[3]) != None):
                print("Nome: " + element[0] +
                    "\nEmail: " + element[3] +
                    "\nProva: " + element[4] +
                    "\n")