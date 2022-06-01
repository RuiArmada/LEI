import re
from collections import Counter, OrderedDict
from os import mkdir, path

def questao5(data):
    teams = dict.fromkeys(set([str(x[6]) if len(x[6]) > 0 or str(x[6]).upper()
               == "INDIVIDUAL" else "individual" for x in data])) 
    #(^(i?[\w]+)$)
    teams = { team : [x for x in data if x[6] == team] for team in teams}

    teams = dict(sorted(teams.items(), key=lambda p: len(p[1]),reverse=True))
    
    if not path.isdir('files-html'):
        mkdir('files-html')

    init = "<!DOCTYPE html>\n<html>\n\t<head>\n\t\t<meta charset=\"UTF-8\"/>\n\t</head>\n\t\t\t<body>\n\t\t\t\t<h1>Prova de Orientação</h1>\n\t\t\t\t\t<h2>Equipas:</h2>\n"
    endf = "\n\t\t\t</body>\n</html>"
    with open("files-html/equipas.html","a") as file:
        file.truncate(0)
        file.write(init)
        count = 1
        for key in teams:
            file.write("\t\t\t<li>\n")
            file.write("<a href=\"equipa"+str(count)+".html\">" + key + " : " + str(len(teams[key])))
            linkEquipa(key,teams[key],count)   
            file.write("\n\t\t\t</li>\n")
            count+=1     

        file.write(endf)


def linkEquipa(equipa,elementos,count):
    with open("files-html/equipa"+str(count)+".html","a") as eqFile:
        init = "<!DOCTYPE html>\n<html>\n\t<head>\n\t\t<meta charset=\"UTF-8\"/>\n\t</head>\n\t\t\t<body>\n\t\t\t\t<h1>Prova de Orientação</h1>\n\t\t\t\t\t<h2>Equipa "+equipa+"</h2>\n"
        endf = "\n\t\t\t</body>\n</html>"
        eqFile.truncate(0)
        eqFile.write(init)
        for inscrito in elementos:
            eqFile.write("\n<li>Nome : " + sanitize(inscrito[0]) + "; \n\nProva(s) : " + sanitize(
                inscrito[4]) + "; \n\nEscalão : " + sanitize(inscrito[5]) + ";</li>\n")
        eqFile.write(endf)


def sanitize(inp):
    inp = re.sub(r"\&", "&amp;", inp)
    inp = re.sub(r"\s", "&nbsp;", inp)
    inp = re.sub(r"\<", "&lt;", inp)
    inp = re.sub(r"\>", "&gt;", inp)
    inp = re.sub(r"\"", "&quot;", inp)
    inp = re.sub(r"\'", "&apos;", inp)
    return inp
