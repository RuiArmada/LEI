:- style_check(-discontigous).
:- style_check(-singleton).
:- op(900,xfy,'::').

% QUESTÃO 1

% a)
/*

Aqui convém dividir o problema em dois predicados:

    1 - Consulta
    2-  Especialidade

Sendo que ambos os predicados serão relacionados pelo nome do médico

    consulta(nome,medico,data).
    especialidade(especialidade,medico).

*/

%b)

especialidade(og,ana).
especialidade(g,bruna).
especialidade(cgpc,carlos).
especialidade(pc,duarte).
especialidade(o,filipe).

% Fazes isto qd aparecem conjuntos p.e. {bruh1,bruh2}
excecao(especialidade(cg,eduardo)).
excecao(especialidade(pc,eduardo)).
%

% Fazes isto qd aparecem cenas esquesitas do genero "@FG"
especialidade(x,guilherme).
excecao(_,M) :- especialidade(x,M).
nulo(x).
+especialidade(E,M) :- (findall(Especialidade,especialidade(Especialidade,guilherme),not(nulo(Especialidade)),S),length(S,N),N==0).
%

consulta(rosa,ana,segunda).
consulta(paulo,carlos,terca).

excecao(consulta(sara,ana,quarta)).
excecao(consulta(sara,bruna,quarta)).

% Fazes isto qd pararece #
consulta(tavares,duarte,x2).
excecao(N,M,_) :- consulta(P,M,x2).
%

consulta(urbano,eduardo,quinta).

% Fazes isto quando aparecem intervalos p.e. [start,end]
excecao(rosa,ana,D) :- member(D,[segunda,terca,quarta,quinta,sexta]).
%

excecao(consulta(vasco,filipe,sabado)).
excecao(consulta(vasco,filipe,domingo)).

% c)

+consulta(N,M,D) :: (D\=feriado, findall(D,consulta(P,M,D),S),length(S,N),N==1).

% d)

-especialidade(E,M) :: (findall(M,especialidade(E,M),S),length(S,N),N==0).

% QUESTÃO 2

% a)

    % consulta2(Nome,Medico,Especialidade,Data).

    insercao(Termo) :- assertz(Termo).
    insercao(Termo) :- retract(Termo), !, fail.

    remocao(Termo) :- retract(Termo).
    remocao(Termo) :- assertz(Termo), !, fail.

    teste([]).
    teste([X|Xs]) :- X, teste(Xs).

    demo(Questao, verdadeiro) :- Questao.
    demo(Questao, falso) :- -Questao.
    demo(Questao,desconhecido) :- not(Questao), not(-Questao).

    evolucao(Termo) :- findall(Invariante,+Termo::Invariante,Lista),insercao(Termo),teste(Lista).

    involucao(Termo) :- findall(Invariante,-Termo::Invariante,Lista),remocao(Termo),teste(Lista).

% b)

    %i)
        ? - consulta2(rosa,X,o,segunda).

    %ii)

        ? - consulta2(ines,xavier,g,X).

% QUESTAO 3

/*

    a) Verdadeira

       Operador Ubiversas is the reason

    b) Falsa

        O findall qd nao encontra nada na sua pesquisa retorna uma lista bazia, enquanto que o setof
        dá mesmo falso quando não consegue encontrar nada na pesquisa

    c) Falsa

        O tesmo is força o cálculo de uma expressão aritmentica. Neste caso, X é o resultado da expressão aritmentica X+1

    d) Falsa

        Os invariantes destinam-se à proibição de inserção de conhecimento incorreto

    e) Falsa

        Pode-se abandonar o Pressuposto do Mundo Fechado, adotar a Perspetiva do Mundo Aberto, em que 
        podem existir outros factos ou conclusões verdadeiras para além dos representados na base de conhecimento

    f) Falsa

        Um valor nulo do tipo incerto identifica um conjunto infinito de valores

*/