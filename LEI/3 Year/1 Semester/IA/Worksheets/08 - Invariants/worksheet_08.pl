%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% ITELIGENCIA ARTIFICIAL - MiEI/LEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic son/2.
:- dynamic pai/2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado filho: Filho,Pai -> {V,F,D}

son( joao,jose ).
son( jose,manuel ).
son( carlos,jose ).

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido

+son( F, P ) :: (
                 sol( ( F, P ), 
                 (son( F, P )), S ),
                 comp( S, N ), 
				 N == 1
                ).

% Invariante Referencial: nao admitir mais do que 2 progenitores
%                         para um mesmo individuo

+son( F, P ) :: (   
				sol( ( F, P ), ( son( F, P ), S )),
                comp( S, N ),
                N == 1     
               ).

+son( F, _ ) :: (
                sol( Ps, ( son( F, Ps ), S )),
                comp( S, N ),
                N =< 2
                ).

-son( F, P ) :: (
                sol( F,( age( F, I ) ), S),
                comp( S, N ),
                N == 0
                ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a evolucao do conhecimento

evolution( Term ) :- sol( Invariant, +Term::Invariant, List ), 
                     inser(Term),
                     test(List).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a involucao do conhecimento

involution( Term ) :- sol( Invariant, +Term::Invariant, List ),
                      rem(Term),
                      test(List).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Auxiliar

test( [] ).
test( [R|Lr] ) :- R, test(Lr).

sol( X, P, S ) :- findall( X, P, S ).

rem( Term ) :- retract( Term ).
rem( Term ) :- assert( Term ), !, fail.

comp( S, N ) :- length( S, N ).

inser( Term ) :- assert( Term ).
inser( Term ) :- retract( Term ), !, fail.
