%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Inteligência Artificial MIEI /3  LEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica 
% Conhecimento Imperfeito (Ficha 10)

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

:- op(900,xfy,'::').
:- dynamic game/3.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

-game( Game, Referee, Help ) :- mynot( game( Game, Referee, Help ) ),
                                mynot( excep( game( Game, Referee, Help ) ) ).

game( 1, aa, 500 ).
game( 2, bb, xpto0123).

excep( game( Game, Referee, _ ) ) :- game( Game, Referee, xpto0123 ).

excep( game( 3, cc, 500 ) ).
excep( game( 3, cc, 2500 ) ).

excep( game( 4, dd, Help ) ) :- Help >= 250, Help =< 750.

game( 5, ee, xpto765 ).

except( game( Game, Referee, _ ) ) :- game( Game, Referee, xpto765 ).

null(xpto765).

+game( G, R, H ) :: ( mysol( Help, ( game( 5, ee, Help ), mynot( null( Help ) ) ), S ), N == 0 ).

game( 6, ff, 250 ).

excep( game( 6, ff, Help ) ) :- Help > 5000.

-game( 7, gg, 2500 ).

game( 7, gg, xpto768 ).

excep( game( Game, Referee, _ ) ) :- game( Game, Referee, xpto768 ).

cerca( Value, Sup, Inf ) :- Sup is Value * 1.25,
                            Inf is Value * 0.75.

excep( game( 8, hh, Value ) ) :- cerca( 1000, Sup, Inf ),
                                 Value >= Inf,
                                 Value =< Sup.

prox( Value, Sup, Inf ) :- Sup is Value * 1.10,
                           Inf is Value * 0.90.

excep( game( 9, ii, Value ) ) :- prox( 3000, Sup, Inf ),
                                 Value >= Inf,
                                 Value =< Sup.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Evolucao & Involucao

evolucao( Term ) :- mysol( Invariant, +Term::Invariant, List ), myinsert( Term ), mytest( List ).

involucao( Term ) :- mysol( Invariant, -Term::Invariant, List ), myrem( Term ), mytest( List ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Utils

myinsert( Term ) :- assertz( Term ).
myinsert( Term ) :- retract( Term ),!,fail.

mytest( [] ).
mytest( [R|LR] ) :- R, mytest( LR ).

mynot( Question ) :- Question, !, fail.
mynot( _ ).

myrem( Term ) :- retract( Term ).
myrem( Term ) :- assertz( Term ),!,fail.

mysol( X,Y,Z ) :- findall( X,Y,Z ).

mycomp( S,N ) :- length( S,N ).

demo( Question, verdadeiro ) :- Question.
demo( Question, falso ) :- -Question.
demo( Question, desconhecido ) :- mynot( Question ), mynot( -Question ).
