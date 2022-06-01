%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Inteligência Artificial - MiEI/3 LEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Operacoes aritmeticas.

ope( X+Y,R ) :- R is X+Y.
ope( X-Y,R ) :- R is X-Y.
ope( X*Y,R ) :- R is X*Y.
ope( X/Y,R ) :- R is X/Y.

max( X,Y,R ) :- X>=Y , R is X.
max( X,Y,R ) :- Y>=X , R is Y.

max3( X,Y,Z,R ) :- max( X,Y,L ) , max( L,Z,R ).

min( X,Y,R ) :- X>=Y , R is Y.
min( X,Y,R ) :- Y>=X , R is X.

min3( X,Y,Z,R ) :- min( X,Y,L ) , min( L,Z,R ).

par( X ) :- 0 is mod( X,2 ).

imp( X ) :- not(par( X )).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado soma: X,Y,Soma -> {V,F}

soma( X,Y,Soma ) :- Soma is X+Y.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado soma: X,Y,Z,Soma -> {V,F}

soma( X,Y,Z,Soma ) :- Soma is X+Y+Z.