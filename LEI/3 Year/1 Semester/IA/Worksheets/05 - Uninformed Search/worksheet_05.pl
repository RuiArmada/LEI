%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% inteligência Artificial - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica 
% Pesquisa não Informada (Ficha 5)

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

:- dynamic nao/1.

inicial( jarros( 0,0 ) ).

final( jarros( 4,_ ) ).
final( jarros( _,4 )).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

transicao( jarros( V1, V2 ), encher( 1 ), jarros( 8, V2 ) ) :- V1 < 8.
transicao( jarros( V1, V2 ), encher( 2 ), jarros( V1, 5 ) ) :- V2 < 5.
transicao( jarros( V1, V2 ), esvaziar( 1 ), jarros( 0, V2 ) ) :- V1 > 0.
transicao( jarros( V1, V2 ), esvaziar( 2 ), jarros( V1, 0 ) ) :- V2 > 0.
transicao( jarros( V1, V2 ), encher( 1, 2 ), jarros( NV1, NV2 ) ) :- 
                                                                    V1 > 0,
                                                                    NV1 is max( V1 - 5 + V2, 0 ),
                                                                    NV1 < V1,
                                                                    NV2 is V2 + V1 - NV1.

transicao( jarros( V1, V2 ), encher( 2, 1 ), jarros( NV1, NV2 ) ) :- 
                                                                    V2 > 0,
                                                                    NV2 is max( V2 - 8 + V1, 0 ),
                                                                    V2 < V2,
                                                                    NV1 is V1 + V2 - NV2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

resolvedf( Solucao ) :- inicial( InicialEstado ), resolvedf( InicialEstado, [InicialEstado], Solucao ).
resolvedf( Estado, Historico, [] ) :- final( Estado ), !, write( Historico ).
resolvedf( Estado, Historico, [Move|Solucao] ) :- transicao( Estado, Move, Estado1 ), nao( membro( Estado1, Historico ) ), resolvedf( Estado1, [Estado1|Historico], Solucao ).
resolvebf(Solucao) :- resolvebf2(jarros(0,0),jarros(_,4),Solucao).


resolvebf2( EstadoI, EstadoF, Solucao ) :- bfs3( EstadoF, [[EstadoI]], Solucao ).

bfs3( EstadoF, [[EstadoF|T]|_], Solucao ) :- reverse( [EstadoF|T], Solucao ).
bfs3( EstadoF, [EstadoA|Outros], Solucao) :- reverseEstadosA = [Act|_],
                                             findall( [EstadoX|EstadosA], ( Estado\==Act, transicao( Act, Move, EstadoX ),
                                             \+member( Estadox, EstadosA ) ), Novos ),
                                             append( Outros, Novos, Todos ),
                                             bfs3( EstadoF, Todos, Solucao ).
