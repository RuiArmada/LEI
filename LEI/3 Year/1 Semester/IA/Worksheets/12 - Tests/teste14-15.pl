:- style_check(-discontigous).
:- style_check(-singleton).
:- op(900,xfy,'::').

% -----------------------------------
%            QUESTÃO 1
% -----------------------------------

% a)

/*
    12345: Há registo de mais um facto
    23456: Há registo de mais um facto
    34567: Conhecimento impreciso porque sabe-se que o número de créditos feitos é menor que 45.
    45678: Conhecimento impreciso porque sabe-se que o número de anos efetuados encontra-se entre 3 e 5.
    56789: Conhecimento incerto pois não se sabe se esse aluno pagou a propina
    67890: Conhecimento impreciso pois o curso e ECTS realizados são de um conjunto de hipóteses
    78901: Conhecimento impreciso pois os nomes são de um conjunto de possibilidades
    89012: Conhecimento interdito pois não sabemos o curso nem podemos vir a saber.
    90123: Conhecimento incerto  pois não sabemos qual é o curso.

    Ex:
        aluno(ID,Nome,Curso,Ano,ECTS,Propina).
*/

% b)
    
    aluno(12345,ana,lei,1,60,sim).
    aluno(23456,beatriz,lcc,2,60,nao).
    excecao(aluno(34567,carlos,mdi,1,ECTS,sim)) :- ECTS >= 0, ECTS < 45.
    excecao(aluno(45678,duarte,miec,Ano,180,sim)) :- member(Ano,[3,4,5]).
    aluno(56789,eva,miec,4,240,prop).
    excecao(I,N,C,A,E,P) :- aluno(I,N,C,A,E,prop).
    excecao(aluno(67890,filipe,lfis,1,45,nao)).
    excecao(aluno(67890,filipe,lfis,1,54,nao)).
    excecao(aluno(67890,filipe,lefis,1,45,nao)).
    excecao(aluno(67890,filipe,lefis,1,54,nao)).
    excecao(aluno(78901,gisela,lcc,3,180,sim)).
    excecao(aluno(78901,gisele,lcc,3,180,sim)).
    excecao(aluno(78901,gabriel,lcc,3,180,sim)).
    aluno(89012,heitor,curso,1,10,nao).
    excecao(I,N,C,A,E,P) :- aluno(I,N,curso,A,E,P).
    nulo(curso).
    +aluno(I,N,C,A,E,P) :: (findall(Cursos,(aluno(89012,_,Cursos,_,_,_),not(nulo(Cursos))),S),length(S,N1),N1==0).
    aluno(90123,ivo,curs,2,180,sim).
    excecao(I,N,C,A,E,P) :- aluno(I,N,curs,A,E,P).

% c)

    +aluno(I,N,C,A,E,P) :: (A =< 5, E =< 300).

% d) 

    insercao(Termo) :- assertz(Termo).
    insercao(Termo) :- retract(Termo),!,fail.

    remocao(Termo) :- retract(Termo).
    remocao(Termo) :- assertz(Termo),!,fail.

    teste([]).
    teste([X|Xs]) :- X,teste(XS).

    evolucao(Termo) :- findall(Invariante,+Termo::Invariante,Lista),
                       insercao(Termo),
                       teste(Lista).

    involucao(Termo) :- findall(Invariante,-Termo::Invariante,Lista),
                        remocao(Termo),
                        teste(Lista).

% -----------------------------------
%            QUESTÃO 2
% -----------------------------------

/*

a) Falso

   Operador Universal

b) Falso

   Compara os dois valores apenas

c) Falso

   Negação Forte e Explicíta são duas situações para expressa algo que é falso
   Negação por Falha na prova representa situações para as quais não existe prova de que
   sejam verdadeiras

e) Verdadeiro

f) Falso

    Invariantes efetuam testes à consistência e integridade

*/