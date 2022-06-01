%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% ITELIGÊNCIA ARTIFICIAL - MiEI/LEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica estendida


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% PROLOG: definicoes iniciais

:- dynamic excec/1, '-'/1.
:- dynamic mammal/1.
:- dynamic bat/1.



%--------------------------------- - - - - - - - - - -  -  -  -  -   -

fly( X ) :- bird( X ), no(excec( fly( X ) )).

-fly( tweety ).

-fly( X ) :- mammal( X ), no( excec( -fly( X ) ) ).

-fly( X ) :- excec( fly( X ) ).

fly( X ) :- excec( -fly( X ) ).

bird(tweety).
bird( X ) :- pigeon( X ).
bird( X ) :- ostrich( X ).
bird( X ) :- penguin( X ).

pigeon(gina).
ostrich(trux).
penguin(pingu).

mammal(silvestre).

mammal( X ) :- dog( X ).
mammal( X ) :- cat( X ).
mammal( X ) :- bat( X ).

bat(batman).
dog(budy).
cat(garfield).

si( Question, true ) :- Question.
si( Question, false ) :- -Question.
si( Question, unknown ) :- no( Question ), no( -Question ).

siL( [], [] ).
siL( [Question|L], [Answer|S] ) :- si( Question, Answer ),
                                   siL( L, S ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

no( Question ) :-
    Question, !, fail.
no( Question ).
