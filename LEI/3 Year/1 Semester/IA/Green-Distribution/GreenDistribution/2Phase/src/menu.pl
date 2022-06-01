:- consult([graph,transportes,clientes,estafetas,encomendas,arestas,nodos,estimativas,auxiliares,algoritmos,predicados2]).
:- dynamic doit/1.

%============================================================================================
%                                       MENU

menu :- repeat,
    write('                        '),nl,
    write('+----------------------------------------------------------------------------------------------------+'),nl,
    write('|  GREEN DISTRIBUTION                                                                                |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  1 | Gerar circuitos de entrega, caso existam, que cubram um determinado territorio                |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  2 | Visualizar nodos adjacentes de um ponto de entrega                                            |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  3 | Identificar quais os circuitos com maior numero de entregas                                   |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  4 | Comparar circuitos de entrega tendo em conta os identificadores de produtividade              |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  5 | Escolher o circuito mais rapido, utilizando o criterio da distancia                           |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  6 | Escolher o circuito mais ecologico, utilizando o criterio de tempo                            |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  7 | Visualizar graficamente o grafo                                                               |'),nl,
    write('+----+-----------------------------------------------------------------------------------------------+'),nl,
    write('|  0 | Sair do programa                                                                              |'),nl,
    write('+----------------------------------------------------------------------------------------------------+'),nl,
    write('Escolha uma opcao: '),nl,
    read(Choice),
    0=<Choice,
    Choice=<7,
    (Choice == 0 -> write('A sair do programa...'), nl, doit(Choice); doit(Choice)),
    sleep(2),
    Choice=0,
    !.

doit(1) :-
    write('                        '),nl,
    write('Insira o numero de um nodo:'),nl,
    read(R), R>=0,
    write('                        '),nl,
    circuito(R,N),
    (length(N,0) -> writeln('Não existem caminhos para esse nodo.') ; write('Circuitos Gerados: '),write(N),nl)
    ,!.

doit(2) :- 
    write('                        '),nl,
    write('Insira o numero de um nodo:'),nl,
    read(R), R>=0,
    write('                        '),nl,
    adjacentes(R,N),
    write('Pontos de Entrega Adjacentes: '),write(N),nl,
    !.

doit(3) :-
    pred3(L),
    write('                        '),nl,
    write('Circuitos com maior numero de entregas: '),nl,nl,q3Print(L),nl,
    !.

doit(4) :-
    write('                        '),nl,
    write('Insira o seu Destino:'),nl,
    read(D), D>=0,
    write('                        '),nl,
    write('Insira o Peso da encomenda:'),nl,
    read(P), P>=0,
    write('                        '),nl,
    write('Insira o Tempo:'),nl,
    read(T), T>=0,
    write('                        '),nl,
    maisEficiente(D,P,T,(V,(C,Dm))),
    write('Veiculo: '),write(V),nl,
    write('Caminho: '),write(C),nl,
    write('Distancia: '),write(Dm),nl,
    !.

doit(5) :- 
    write('                        '),nl,
    write('Insira o numero do nodo final:'),nl,
    read(R), R>=0,
    write('                        '),nl,
    menorDist(R,X),
    write('Circuito mais rapido, utilizando o criterio de distancia: '),write(X),nl,
    !.

doit(6) :-
    write('                        '),nl,
    write('Insira o numero do nodo final:'),nl,
    read(R), R>=0,
    write('                        '),nl,
    write('Insira o Peso da encomenda: '),nl,
    read(P), P>=0,
    write('                        '),nl,
    maisFast(R,P,(V,C,T)),
    write('Veiculo: '),write(V),nl,
    write('Caminho: '),write(C),nl,
    write('Tempo: '),format('~2f Horas\n', [T]),nl,
    !.

doit(7) :-
  shell('sh view.sh'),
  !.

doit(0) :- 
    sleep(1),
    halt,
    !.
