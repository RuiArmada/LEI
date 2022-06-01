%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Inteligência Artificial MIEI /3  LEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica 
% Pesquisa Informada (Ficha 6)

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento

:- discontiguous primProf/3.
:- style_check(-singleton).

estimate( a,5 ).
estimate( b,4 ).
estimate( c,4 ).
estimate( d,3 ).
estimate( e,7 ).
estimate( f,4 ).
estimate( g,2 ).
estimate( s,10 ).
estimate( t,0 ).

edge( s,a,2 ).
edge( a,b,2 ).
edge( b,c,2 ).
edge( c,d,3 ).
edge( d,t,3 ).
edge( s,e,2 ).
edge( e,f,5 ).
edge( f,g,2 ).
edge( g,t,2 ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

not( Questao ) :- Questao, !, fail.
not( Questao ).

member( X, [X|_] ).
member( X, [_|Xs] ) :- member( X, Xs ).

invert( Xs, Ys ) :- invert( Xs, [], Ys ).

invert( [], Xs, Xs ).
invert( [X|Xs], Ys, Zs ) :- invert(Xs, [X|Ys], Zs).

min( [(P,X)], (P,X) ).
min( [(P,X)|L], (Py,Y) ) :- min( L, (Py,Y) ), X>Y.
min( [(Px,X)|L], (Px,X) ) :- min( L, (Py,Y) ), X=<Y.

select( E, [E|Xs], Xs ).
select( E, [X|Xs], [X|Ys] ) :- select( E, Xs, Ys ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

edge( Node, Next ) :- edge( Node, Next, _ ).

start( s ).

end( t ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

adjacent( Node, Next ) :- edge( Node, Next, _ ).
adjacent( Node, Next ) :- edge( Next, Node, _ ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

primProf( Node, _, [] ) :- end( Node ).

solvePrimProf( Node, [Node|Path] ) :- primProf( Node, [Node], Path ).

primProf( Node, Hist, [Next|Path] ) :- adjacent( Node, Next ),
                                       not( member( Next, Hist) ),
                                       primProf( Next, [Next|Hist], Path ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

allSol( L ) :- findall( ( S, C ),
               (solvePrimProf( s, S )),
               length( S, Int),
               L ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

prof( Dest, Dest, H, D ) :- invert( H,D ).
prof( Source, Dest, His, C ) :- adjacent( Source, Nex ),
                                not( member( Nex, His ) ),
                                prof( Nex, Dest, [Nex|His], C ).
 
solvPPMult( NodeStart, NodeEnd, [Node|Path] ) :- prof( NodeStart, NodeEnd, [NodeStart], Path ).

adjacentCost( Node, Next, Cost ) :- edge( Node, Next, Cost ).
adjacentCost( Node, Next, Cost ) :- edge( Next, Node, Cost ).

primProfCost( Node, _, [], 0 ) :- end(Node).
primProfCost( Node, Hist, [Next|Path], Cost ) :- adjacentCost( Node, Next, CostMov ),
                                                 not( member( Next, Hist) ),
                                                 primProfCost( Next, [Next|Hist], Path, Cost2 ),
                                                 Cost is CostMov + Cost2.

solvePrimProfCost( Node, [Node|Path], Cost ) :- primProfCost( Node, [Node], Path, Cost ). 

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

ppCostAllSol( L ) :- findall( ( S, C ),
                     (solvePrimProfCost( s, S, C )),
                     L ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

best( Node, Cam, Cost ) :- findall( ( Ca, Cos ),
                           solvePrimProfCost( Node, Ca, Cos ),
                           L ),
                           min( L,
                               ( Cam, Cost ) ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

getBestG( [Path], Path ) :- !.
getBestG( [Path1/Cost1/Est1,_/Cost2/Est2|Paths], Best ) :- Est1 =< Est2,
                                                           !,
                                                           getBestG([Path1/Cost1/Est1|Paths], Best).
getBestG([_|Paths], Best) :- getBestG( Paths, Best ).                                

adjacentG( [Nodo|Path]/Cost/_, [Next,Node|Path]/New/Est ) :- edge( Node, Next, PassCost ),
                                                             \+ member( Next, Path ),
                                                             New is Cost + PassCost,
                                                             estimate( Next, Est ).

bigGreed( Path, ExPaths ) :- findall( New,
                                      adjacentG( Path, New ),
                                      ExPaths ).

greedy( Paths, Path ) :- getBestG( Paths, Best ),
                         select( Best, Paths, Other ),
                         bigGreed( Best, ExPaths ),
                         append( Other, ExPaths, NewPaths ),
                         greedy( NewPaths, SolPath ).

solveGreed( Node, Path/Cost ) :- estimate( Node, Estimate ),
                                 greedy( [[Node]/0/Estimate], InvPath/Cost/_ ),
                                 invert( InvPath, Path ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

getBest( [Path], Path ) :- !.
getBest( [Path1/Cost1/Est1,_/Cost2/Est2|Paths], Best ) :- Cost1 + Est1 =< Cost2 + Est2,
                                                          !,
                                                          getBest( [Path1/Cost1/Est1|Paths], Best ).
getBest( [_|Paths], Best ) :- getBest( Paths, Best ).          

bigStar( Path, ExPaths ) :- findall( New,
                                     adjacentG( Path, New ),
                                     ExPaths ).

star( Paths, Path ) :- getBest( Paths, Path ), 
                       Path = [Node|_]/_/_, 
                       end( Node ).

star( Paths, SolPath ) :- getBest( Path, Best ),
                          select( Best, Path, Other ),
                          bigStar( Best, ExPaths ),
                          append( Other, ExPaths, NewPaths ),
                          star( NewPaths, SolPath ).

solveStar( Node, Path/Cost ) :- estimate( Node, Estimate ),
                                star( [[Node]/0/Estimate], InvPath/Cost/_ ),
                                invert( InvPath, Path ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -                            