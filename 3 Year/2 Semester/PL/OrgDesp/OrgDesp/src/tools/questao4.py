import re

def questao4(data):
    escaloes = dict()
    for elem in data:
        escalao = elem[5]
        if escalao in escaloes:
            escaloes[escalao] += 1
        else:
            escaloes[escalao] = 1
    
    for key, value in dict(sorted(escaloes.items(), key=lambda p: p[0])).items():
            if key == "":
                key = 'Sem escalao'
            print("Escalao: " + key + "  |  Contagem:" + str(value))    