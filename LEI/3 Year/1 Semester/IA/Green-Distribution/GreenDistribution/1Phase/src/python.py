#!/usr/bin/env python3

def open_transportes():
    with open('transportes.pl') as infile:
        transportes = []
        for i in infile:
            data = i.replace('(',',',1).replace('(', '').replace(')', '')
            content = data.split(",",1)
            tra = content[1].split(',')
            transportes.append([tra[0].replace(' ',''),tra[1].replace(' ',''),tra[2].replace(' ',''),tra[3].replace(' ',''),tra[4].replace(' ','').strip("\n")])
    return transportes

def open_clientes():
    with open('clientes.pl') as infile:
        clientes=[]
        for i in infile:
            data = i.replace('(',',',1).replace('(', '').replace(')', '')
            content = data.split(",",1)
            cli = content[1].split(',',2)
            clientes.append([cli[0].replace(' ',''),cli[1],cli[2].replace(' ','').strip("\n")])
    clientes_grouped = [clientes[10*i:(10*i)+10] for i in range(51)]
    return clientes_grouped

def open_estafetas():
    with open('estafetas.pl') as infile:
        estafetas = []
        for i in infile:
            data = i.replace('(',',',1).replace('(', '').replace(')', '')    
            content = data.split(",",1)
            est = content[1].split(',',2)
            estafetas.append([est[0].replace(' ',''),est[1],est[2].replace(' ','').strip("\n")])
    estafetas_grouped = [estafetas[10*i:(10*i)+10] for i in range(31)]
    return estafetas_grouped

def open_encomendas():
    with open('encomendas.pl') as infile:
        encomendas = []
        for i in infile:                            
            data = i.replace('(',',',1).replace('(', '').replace(')', '')
            content = data.split(",",1)
            enc = content[1].split(',')
            encomendas.append([enc[0].replace(' ',''),enc[1].replace(' ',''),enc[2].replace(' ',''),enc[3].replace(' ',''),enc[4].replace(' ',''),enc[5].replace(' ',''),enc[6].replace(' ',''),enc[7].replace(' ',''),enc[8].replace(' ',''),enc[9].replace(' ',''),enc[10].replace(' ',''),enc[11].replace(' ','').strip(".\n")])
        encomendas_grouped = [encomendas[25*i:(25*i)+25] for i in range(501)]
    return encomendas_grouped

def main():
    enc = open_encomendas()
    est = open_estafetas()
    cli = open_clientes()
    transportes = open_transportes()
    x = 0
    a = 1
    z = input("Qual a tabela de conhecimento que pretende imprimir (clientes,estafetas,encomendas,transportes)\n > ")
    while z != "exit":
        if z == "encomendas":
            x = 0
            while x < 201:
                page = enc[x]
                print("-------------------------------------------------------- Encomendas --------------------------------------------------------")
                for row in page:
                    print("+---------------------------------------------------------------------------------------------------------------------------")
                    print("|   encomenda("+row[0]+",("+row[1]+','+row[2]+','+row[3]+"),("+row[4]+','+row[5]+"),("+row[6]+','+row[7]+','+row[8]+','+row[9]+"),("+row[10]+','+row[11]+")).")
                print("+---------------------------------------------------------------------------------------------------------------------------")
                a = input("Página {} de 200   (exit para sair)\n Ir para > ".format(x+1))
                if (a == "exit"):
                    a = 800
                x = int(a)-1
        elif z == "estafetas":
            x = 0
            while x < 31:
                page = est[x]
                print("-------------------------------------------------------- Estafetas ---------------------------------------------------------")
                for row in page:
                    print("+---------------------------------------------------------------------------------------------------------------------------")
                    print("|   estafeta("+row[0]+','+row[1]+','+row[2]+").")
                print("+---------------------------------------------------------------------------------------------------------------------------")
                a = input("Página {} de 30   (exit para sair)\n Ir para > ".format(x+1))
                if (a == "exit"):
                    a = 800
                x = int(a)-1
        elif z == "clientes":
            x = 0
            while x < 51:
                page = cli[x]
                print("-------------------------------------------------------- Clientes ---------------------------------------------------------")
                for row in page:
                    print("+---------------------------------------------------------------------------------------------------------------------------")
                    print("|   cliente("+row[0]+','+row[1]+','+row[2]+").")
                print("+---------------------------------------------------------------------------------------------------------------------------")
                a = input("Página {} de 50   (exit para sair)\n Ir para > ".format(x+1))
                if (a == "exit"):
                    a = 800
                x = int(a)-1
        elif z == "transportes":
                print("------------------------------------------------------- Transportes --------------------------------------------------------")
                for row in transportes:
                    print("+---------------------------------------------------------------------------------------------------------------------------")
                    print("|   transporte("+row[0]+','+row[1]+','+row[2]+','+row[3]+','+row[4]+").")
                print("+---------------------------------------------------------------------------------------------------------------------------")
        print("Para sair escreva exit")
        z = input("Qual a tabela de conhecimento que pretende imprimir (clientes,estafetas,encomendas,transportes)\n > ")


if __name__ == "__main__":
    main()
