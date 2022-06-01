:- style_check(-discontigous).
:- style_check(-singleton).
:- op(900,xfy,'::').

% -----------------------------------
%            QUESTÃO 1
% -----------------------------------

% a)

/*
    Aqui convém separar o conhecimento em dois predicados:
        1- para a especialidade
        2- para a consulta
    Sendo que ambos os predicados estarão relacionados pelo nome do doutor/doutora.

    Ex:
        especialidade(Especialidade,Medico).
        consulta(Paciente,Medico,Data).
*/

% b)

    especialidade(obstetricia_ginecologia,ana).
    especialidade(ginecologia,bruna).
    especialidade(cirurgiageral_pequenacirurgia, carlos).
    especialidade(pequenacirurgia, duarte).
    especialidade(cirurgiageral_pequenacirurgia,eduardo).
    especialidade(ortopedia,filipe).

    % CASO 1: Não se sabe a especialidade (conhecimento interdito)
    especialidade(desc,guiherme). % desc = fator desconhecido do predicado
    excecao(_,M) :- especialidade(desc,M). % M = Nome do Médico
    nulo(desc).
    +especialidade(E,M) :- (findall(Especialidade,especialidade(Especialidade,guiherme),not(nulo(Especialidade)),S),length(S,N),N==0).

    consulta(rosa,ana,segunda).
    consulta(paulo,carlos,terca).

    % CASO 2: Nao se sabe quem atenderá a Sara (conhecimento impreciso)
    excecao(consulta(sara,ana,quarta)).
    excecao(consulta(sara,bruna,quarta)).

    % CASO 3: Incerteza na data (conhecimento incerto)
    consulta(tavares,duarte,desc2). % desc2 = fator descinhecido do predicado
    excecao(P,M,_) :- consulta(P,M,desc2).

    consulta(urbano,eduardo,quinta).

    %CASO 4: Intervalo de datas 
    excecao(rosa,ana,D) :- member(D,[segunda,terca,quarta,quinta,sexta]).

    %CASO 5: Nao se sabe quando se atenderá o Vasco (conhecimento impreciso)
    excecao(consulta(vasco,filipe,sabado)).
    excecao(consulta(vasco,filipe,domingo)).

% c)

    +consulta(P,M,D) :: (D\=feriado, findall(D,consulta(P,M,D),S),length(S,N),N==1).

% d)

    -especialidade(E,M) :: (findall(M,especialidade(E,M),S),length(S,N),N==0).

% -----------------------------------
%            QUESTÃO 2
% -----------------------------------

% a)

    % consulta(Paciente,Medico,Especialidade,Data).

% b)
    % i)
    
        ? - consulta(rosa,X,obstetricia,segunda).
    
    % ii)

        ? - consulta(ines,xavier,geriatria,X).

    insercao(Termo) :- assertz(Termo).
    insercao(Termo) :- retract(Termo), !, fail.

    remocao(Termo) :- retract(Termo).
    remocao(Termo) :- assertz(Termo), !, fail.

    teste([]).
    teste([X|Xs]) :- X, teste(Xs).

    demo(Questao, verdadeiro) :- Questao.
    demo(Questao, falso) :- -Questao.
    demo(Questao,desconhecido) :- not(Questao),not(-Questao).

    evolucao(Termo) :- findall(Invariante,+Termo::Invariante,Lista),insercao(Termo),teste(Lista).

    involucao(Termo) :- findall(Invariante,-Termo::Invariante,Lista),remocao(Termo),teste(Lista).

% -----------------------------------
%            QUESTÃO 3
% -----------------------------------

/*

a) Verdadeira     
     
   Lista é uma lista cuja cabeça é o functor do termo e os argumentos restantes são os argumentos
   do termo. Qualquer lado do predicado pode ser uma variável, mas não ambos. Este predicado é
   denominado "Univ".
   O operador =.. cria a partir da lista dada um predicado com o primeiro elemento da lista, sendo
   os restantes os seus argumentos. Assim A=..[q,a,b,c] fica q(a,b,c).

b) Falsa
     
   O findall quando não encontra nada na sua pesquisa retorna lista vazia, enquanto que o setof
   dá falso quando não consegue encontrar nada na sua pesquisa.

c) Falsa
     
   O termo is força o cálculo de uma expressão aritmética. Neste caso, verifica-se se nesse estado
   X vai corresponder X+1.

d) Falsa

   Os invariantes destinam-se à proibição de inserção de conhecimento incorreto.

e) Falsa

   Pode-se ao abandonar o Pressuposto do Mundo Fechado adotar a perspetica do Mundo Aberto, em que
   podem existir outros factos ou conclusóes verdadeiras para além dos representados na base de conhecimento.

f) Falsa (?)

   Um valor nulo do tipo incerto identifica um conjunto infinito de vaolers

*/ 