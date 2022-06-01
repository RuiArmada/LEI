:- style_check(-discontigous).
:- style_check(-singleton).
:- op(900,xfy,'::').

% QUESTÃO 1

% a)

/*

    12345 - registo de um facto
    23456 - registo de um facto
    34567 - conhecimento impreciso porque sabe-se que o numero de créditos é menor que 45
    45678 - conhecimento impreciso porque sabe-se que o numero de anos se encontra entre 3 e 5
    56789 - conhecimento incerto pois não se tem a certeza se pagou a propina
    67890 - conhecimento incerto porque não se sabe se é de LFis ou LEFis
    78901 - conhecimento incerto porque não se tem a certeza do nome
    89012 - conhecimento interdito pois nao se sabe o curso e nem podemos vir a saber
    90123 - conhecimento incerto pois nao se sabe o curso

    Ex:

        aluno(id,nome,curso,ano,ECTS,propina)

*/

    % b)

        aluno(12345,ana,lei,1,60,sim).
        aluno(23456,beatriz,lcc,2,60,nao).
        
        excecao(aluno(34567,carlos,mdi,1,E,sim)) :- E>=0,E<45.
        
        excecao(aluno(duarte,miec,A,180,sim)) :- member(Ano,[3,4,5]).
        
        aluno(56789,eva,MIEC,4,240,p).
        excecao(I,N,C,A,E,P) :- aluno(I,N,C,A,E,p).
        
        excecao(67890,filipe,lfis,1,45,nao).
        excecao(67890,filipe,lfis,1,54,nao).
        excecao(67890,filipe,lefis,1,45,nao).
        excecao(67890,filipe,lefis,1,54,nao).
        
        excecao(78901,gisela,lcc,3,180,sim).
        excecao(78901,gisele,lcc,3,180,sim).
        excecao(78901,gabriel,lcc,3,180,sim).
        
        excecao(I,N,C,A,E,P) :- aluno(I,N,curso,A,E,P).
        nulo(curso).
        +aluno(I,N,C,A,E,P) :: findall((Cursos,(aluno(89012,_,Cursos,_,_,_),not(nulo(Cursos))),S),length(S,N1),N1==0).

        aluno(90123,ivo,curs,2,180,sim).
        excecao(I,N,C,A,E,P) :- aluno(I,N,curs,A,E,P).

% c)

    +aluno(I,N,C,A,E,P) :: (A=<5,E=<300).

%d)

    insercao(Termo) :- assertz(Termo).
    insercao(Termo) :- retract(Termo),!,fail.

    remocao(Termo) :- retract(Termo).
    remocao(Termo) :- assertz(Termo),!,fail.

    teste([]).
    teste([X|Xs]) :- X, teste(Xs).

    evolucao(Termo) :- findall(Invariante,+Termo::Invariante,Lista),insercao(Termo),teste(Lista).

    involucao(Termo) :- findall(Invariante,-Termo::Invariante,Lista),remocao(Termo),teste(Lista).

% QUESTÃO 2

/*

a) Falsa

    Op Universal

b) Falso

    Apenas compara dois valores

c) Falsa

    Negação forte e explicita são duas situações para expressar algo que é falso
    Negação por Falha na Prova representa situações para as quais não existe prova de veracidade

e) Verdadeiro

f) Falso

    Invariantes testam a consistência e integridade

*/