:- style_check(-discontiguous).
:- style_check(-singleton).
:- op(900,xfy,'::').

% -----------------------------------
%            QUESTÃO 1
% -----------------------------------

% 1)

/*
 a)

    partida(Orig,Dest,Hora,Min).
    chegada(Orig,Dest,Hora,Min).

*/ 

% b)

    chegada(porto,guimaraes,16,00).
    partida(guimaraes,porto,16,15).
    partida(guimaraes,porto,21,45).
    %Conhecimento Incerto
    chegada(guimaraes,trofa,x,y).
    excecao(O,D,_,_) :- chegada(O,D,x,y).
    %Conhecimento Impreciso
    excecao(chegada(trofa,guimaraes,Xh,Ym)) :- Xh is 17, member(Ym,[0,15,30]).
    %Conhecimento Impreciso
    excecao(partida(guimaraes,trofa,18,45)).
    excecao(partida(guimaraes,fafe,18,45)).
    excecao(partida(guimaraes,braga,18,45)).
    %Conhecimento Incerto
    partida(braga,cancel,19,00).
    excecao(O,_,H,M) :- chegada(O,cancel,H,M).
    %Conhecimento Interdito
    partida(njet,braga,21,00).
    excecao(_,D,H,M) :- partida(njet,D,H,M).
    nulo(njet).
    +partida(O,D,H,M) :- (findall(Orig,(partida(Orig,braga,21,00),not(nulo(Orig))),S),length(S,N),N==0).

    chegada(njet2,braga,21,00).
    excecao(_,D,H,M) :- chegada(njet2,D,H,M).
    nulo(njet2).
    +chegada(O,D,H,M) :- (findall(Orig,(chegada(Orig,braga,21,00),not(nulo(Orig))),S),length(S,N),N==0).

% c)

    -chegada(O,D,H,M) :: (findall((Origem,Dest,Horas,Min),chegada(Origem,Dest,Horas,Min),L),length(S,N),N>0).
    % se houver mais do q 0 então não podes remover

% d)

    +partida(O,D,H,M) :: (O\=D).
    -chegada(O,D,H,M) :: (O\=D).

% 2)

    insercao(Termo) :- assert(Termo).
    insercao(Termo) :- retract(Termo),!,fail.

    remocao(Termo) :- retract(Termo).
    remocao(Termo) :- assert(Termo),!,fail.

    teste([]).
    teste([X|XS]) :- X, teste(XS).

    demo(Quest,verdadeira) :- Quest.
    demo(Quest,falso) :- -Quest.
    demo(Quest,desconhecido) :- not(Quest),not(-Quest).

    evolucao(Termo) :- findall(Invariante, +Termo::Invariante, Lista),
                       insercao(Termo),
                       teste(Lista).

    involucao(Termo) :- findall(Invariante, -Termo::Invariante, Lista),
                        remocao(Termo),
                        teste(Lista).

% -----------------------------------
%            QUESTÃO 2
% -----------------------------------

/*

    a) Falsa   

        Supõe-se o predicado (paulo,silva) = (x,y)
        Neste predicado "paulo" vai ser unificado com X, sendo que "silva" vai ser unificado com Y.
        Trata-se assim da unificação de termos, pelo que a afirmação é verdadeira

    b) Falsa

        A expressão X==2, compara um termo X com 2, isto é, se X é 2.
        Como neste caso o termo é literalmente X e não 2, a comparação dá falso, não sendo feita nunca uma 
        unificação.

    c) Falsa

        Podemos ter por exemplo:
        [gina,feminino(helena),entrega(1234)]

    d) Verdadeira

*/

% -----------------------------------
%            QUESTÃO 3
% -----------------------------------

/*

        1) Falso 
            
            herda, mas herda conhecimento que está nas propriedades. ( pode herdar aquela lista  pode ser a lista de predicados que ele quer herdar)

        2) Falso 
        
            vai falhar no predicado pertence porque qualquer questao testada nao vai pertencer a uma lista vazia, nao avançando para o nivel hierarquico superior.

        3) Falso 
        
            tem capacidade.
        4) Falso
        
             o perdicado não esta incorrecta, alias aquela lista pode ser a propriedades que ele pode querer herdar.
        
        5) Falso 
            
            tem a capacidade de herdar conhecimento se a questao nao pertencer à lista de propriedades a cancelar.

        6) Falso
        
            porque assumindo que o predicado pertence está definido correctamente, a 2ª clausula de uma vai ser o oposto da 2ª clausula da outra, produzindo resultados diferentes.

        7) Falso
            
            tem capacidade.

*/