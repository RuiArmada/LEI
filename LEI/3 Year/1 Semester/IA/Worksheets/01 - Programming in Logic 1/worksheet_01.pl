%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Inteligência Artificial MIEI /3  LEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento com informacao genealogica.

filho( joao,jose ).
filho( jose,manuel ).
filho( carlos,jose ).

pai( paulo,jose ).
pai( paulo,maria ).

avo( antonio,paulo ).

neto( nuno,ana ).

sexo( masculino,joao ).
sexo( masculino,jose ).
sexo( feminino,maria ).
sexo( feminino,joana ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado filho: Filho,Pai -> {V,F}

filho( F,P ) :- pai( P,F ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pai: Pai,Filho -> {V,F}

pai( P,F ) :- filho( F,P ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado avo: Avo,Neto -> {V,F}

avo( N,A ) :- filho( A,X ) , pai( N,X ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado bisavo: Bisavo,Bisneto -> {V,F}

bisavo( B,N ) :- descendente( N,B,3 ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado trisavo: Trisavo,Trisneto -> {V,F}

trisavo( T,N ) :- descendente( N,T,4 ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendente: Descendente,Ascendente -> {V,F}

descendente( D,A ) :- filho( D,A ).
descendente( D,A ) :- filho( D,X ) , descendente( X,A ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendente: Descendente,Ascendente,Grau -> {V,F}

descendente( X,Y,1 ) :- filho( X,Y ).
descendente( X,Y,N ) :- filho( X,Z ) , G is N-1 , descendente( Z,Y,G ).




