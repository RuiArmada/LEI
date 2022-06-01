%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Inteligência Artificial MIEI /3  LEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica 
% Conhecimento Imperfeito (Ficha 11)

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

:- op(900.xfy,'::').
:- dynamic service/2.
:- dynamic act/4.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

service( orthopedics, marco ).
service( obstetrics, ana ).
service( obstetrics, maria ).
service( obstetrics, mariana ).
service( geriatrics, sofia ).
service( geriatrics, gininha ).

-service( Service, Name ) :- mynot( service( Service, Name ) ), 
                             mynot( excep( service( Service, Name ) ) ).

-act( Act, Provider, User, Day ) :- mynot( act( Act, Provider, User, Day ) ), 
                                    mynot( excep( act( Act, Provider, User, Day ) ) ).

service( x007, teostra ).
excep( service( _, E ) ) :- service( np9, E ).

service( np9, lunastra ).
excep( service( _, E ) ) :- service( np9, E ).

null( np9 ).

+service( S, E ) :: ( mysol( S, ( service( S, lunastra ), mysol( null( S ) ) ), L ), mycomp( L, N ), N == 0 ).

act( badge, ana, joana, saturday ).
act( cast, amelia, jose, sunday ).

act( x017, mariana, jose, sunday ).
excep( act( _, E, U, D ) ) :- act( x017, E, U, D ).

act( home, maria, x121, x251 ).
excep( act( A, E, _, _ ) ) :- act( A, E, x121, x251 ).

act( suture, x313, josue, monday ).
excep( act( A, _, U, D ) ) :- act( A, x313, U, D ).

excep( act( home, gininha, joao, monday ) ).
excep( act( home, gininha, jose, monday ) ).
excep( act( suture, maria, josefa, thursday ) ).
excep( act( suture, maria, josefa, friday ) ).
excep( act( suture, mariana, josefa, thurday ) ).
excep( act( suture, mariana, josefa, friday ) ).

excep( act( badge, ana, jaice, D ) ) :- mycontain( D, [monday,thursday,wednesday,tuesday,firday] ).

+act( A, P, U, Data ) :: ( D\=holiday, mysol( ( A, P, U, S ), ( act( A, P, U, D ) ), S ), mycomp( S, N ), N == 1 ).

-service( L, Name ) :: ( mysol( Name, act( _, Name, _, _ ), S ), mycomp( S, N ), N == 0 ).

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

mycontain( X, [X|L] ).
mycontain( X, [Y|L] ) :- X\=Y, mycontain( X, L ).

