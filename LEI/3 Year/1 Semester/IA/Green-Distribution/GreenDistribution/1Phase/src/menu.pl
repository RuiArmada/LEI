:- consult([transportes,estafetas,clientes,encomendas,predicados,utils]).
:- dynamic doit/1.

%============================================================================================
%                                       MENU

menu :-repeat,
    write('                        '),nl,
    write('+----------------------------------------------------------------------------------------------------+'),nl,
    write('|  GREEN DISTRIBUTION                                                                                |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  1 | Id do estafeta que mais utilizou transportes ecológicos                                       |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  2 | Qual o estafeta que entregou determinada encomenda a um cliente                               |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  3 | Ids de clientes servidos por um determinado estafeta                                          |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  4 | Valor faturado num determinado dia                                                            |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  5 | Localidades(código postal) com maior volume de entregas                                       |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  6 | Classificação média de um determinado estafeta                                                |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  7 | Número de entregas feitas pelos diferentes transportes, num determinado intervalo de tempo    |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  8 | Frequências de entregas por um estafeta num determinado intervalo de tempo                    |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  9 | Número de encomendas entregues e não entregues, num determinado intervalo de tempo            |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('| 10 | Peso total transportado por um estafeta num determinado dia                                   |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('| 11 | Imprimir da base de conhecimento com paginação                                                |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('| 12 | Guardar a base de conhecimento atual                                                          |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  0 | Sair do programa                                                                              |'),nl,
    write('+----------------------------------------------------------------------------------------------------+'),nl,
    write('Escolha uma opção: '),nl,
    read(Choice),
    0=<Choice,
    Choice=<12,
    (Choice == 0 -> write('A sair do programa...'),nl, doit(Choice); doit(Choice)),
    sleep(2),
    Choice = 0,!.

doit(1) :-
    ecotrans(N),
    write('                        '),nl,
    write('Resultado -> ID do estafeta que utilizou mais vezes transportes ecológicos: '), write(N),nl.

doit(2) :-
    write('                        '),nl,
    write('Insira um ID de Cliente:'),nl,
    read(Id), Id>0, Id=<500,
    estcliente(Id,N),
    write('                        '),nl,
    write('Resultado -> (ID estafeta, ID respetiva encomenda): '),write(N),nl.

doit(3) :-
    write('                        '),nl,
    write('Insira um ID de estafeta:'),nl,
    read(Id), Id>0, Id=<300,
    servEstaf(Id,N),
    write('                        '),nl,
    write('Resultado -> ID clientes servidos pelo estafeta: '),write(N),nl.

doit(4) :-
    write('                        '),nl,
    write('Insira uma data de um dia:'),nl,
    read(Data),
    valordia(Data,N),
    write('                        '),nl,
    write('Resultado -> Valor faturado na dia inserido: '),write(N),nl.

doit(5) :-
    freqZona(Res),
    write('                        '),nl,
    write('Resultado -> (Localidade, Número de encomendas): '),write(Res),nl.

doit(6) :- 
    write('                        '),nl,
    write('Insira um ID de Estafeta:'),nl,
    read(Id), Id>0, Id=<300,
    classificacaoMedia(Id,N),nl,
    write('                        '),nl,
    write('Resultado -> Classificação de um estafeta: '),write(N),nl.

doit(7) :-
    write('                        '),nl,
    write('Insira a data inicial:'),nl,
    read(Di/Mi/Yi), Di>0, Di=<31, Mi>0, Mi=<12, Yi>0, Yi=<2022,
    write('                        '),nl,
    write('Insira a data final:'),nl,
    read(Df/Mf/Yf), Df>0, Df=<31, Mf>0, Mf=<12, Yf>0, Yf=<2022,
    entregasTransporte(Di/Mi/Yi,Df/Mf/Yf,B,M,C),
    write('                        '),nl,
    write('Resultado - Meio de transporte usado para as entregas entre datas'),nl,
    write('          -> Bicicleta: '),write(B),nl,
    write('          -> Mota     : '),write(M),nl,
    write('          -> Carro    : '),write(C),nl.

doit(8) :-
    write('                        '),nl,
    write('Insira a data inicial:'),nl,
    read(Di/Mi/Yi), Di>0, Di=<31, Mi>0, Mi=<12, Yi>0, Yi=<2022,
    write('                        '),nl,
    write('Insira a data final:'),nl,
    read(Df/Mf/Yf), Df>0, Df=<31, Mf>0, Mf=<12, Yf>0, Yf=<2022,
    entregasEstafeta(Di/Mi/Yi,Df/Mf/Yf,Out),
    write('                        '),nl,
    write('Resultado -> (ID estafeta, Número de entregas):'),write(Out),nl.

doit(9) :- 
    write('                        '),nl,
    write('Insira a data inicial:'),nl,
    read(Di/Mi/Yi), Di>0, Di=<31, Mi>0, Mi=<12, Yi>0, Yi=<2022,
    write('                        '),nl,
    write('Insira a data final:'),nl,
    read(Df/Mf/Yf), Df>0, Df=<31, Mf>0, Mf=<12, Yf>0, Yf=<2022,
    estadoIntrevalo(Di/Mi/Yi,Df/Mf/Yf,E,N),
    write('                        '),nl,
    write('Resultado -> Encomendas entregues    :'),write(E),nl,
    write('          -> Encomendas não entregues:'),write(N),nl.

doit(10) :-
    write('                        '),nl,
    write('Insira um ID de estafeta:'),nl,
    read(Id), Id>0, Id=<300,
    write('                        '),nl,
    write('Insira uma data'),nl,
    read(Data), 
    write('                        '),nl,
    pesototal(Id, Data, N),
    write('Resultado -> Peso total das encomendas:'),write(N),nl.

doit(11) :-
    write('                        '),nl,
    write("Não colocar pontos depois dos inputs"),nl,shell('python python.py').

doit(12) :-
	nl,writeln('Pretende guardar a base de conhecimentos atualmente em memória?'),
	writeln('** Isso tem como consequência a remoção/adição permanente de conhecimento**'),
	write('y/N'),nl,
	read(X),
	(X == 'y' -> saveConhecimento,writeln('Conhecimento guardado...');!).

doit(0):-
	sleep(1),
	halt.
