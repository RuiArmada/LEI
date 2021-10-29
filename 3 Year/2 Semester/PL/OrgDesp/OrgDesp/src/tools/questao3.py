import re

def questao3(data, equipas):
    for equipa in equipas: 
        print(f"Equipa {equipa}:")
        for elem in data:
            if re.search(r'(?i)'+equipa, elem[6]):
                print(elem)