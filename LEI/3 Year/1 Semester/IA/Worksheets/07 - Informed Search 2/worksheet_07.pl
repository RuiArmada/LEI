% I.A. - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica 
% Pesquisa Não Informada e Informada (Ficha 7)

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado edge: LocalidadeO,LocalidadeD, CustoDistancia, CustoTempo -> {V,F}

edge( elvas, borba, 15, 10 ).
edge( borba, estremoz, 15, 10 ).
edge( estremoz, evora, 40, 25 ).
edge( evora, montemor, 20, 15 ).
edge( montemor, vendasnovas, 15, 10 ).
edge( vendasnovas, lisboa, 50, 30 ).
edge( elvas, arraiolos, 50, 30 ).
edge( arraiolos, alcacer, 90, 60 ).
edge( alcacer, palmela, 35, 32).
edge( palmela, barreiro, 25, 20 ).
edge( barreiro, palmela, 35, 32 ).
edge( almada, lisboa, 15, 20 ).
edge( elvas, alandroal, 40, 25 ).
edge( alandroal, redondo, 25, 10 ).
edge( redondo, monsaraz, 30, 20 ).
edge( monsaraz, barreiro, 120, 60 ).
edge( barreiro, baixabanheira, 5, 5 ).
edge( baixabanheira, moita, 7, 6 ).
edge( moita, alcochete, 20, 20 ).
edge( alcochete, lisboa, 20, 15 ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado estimate: Localidade, EstimaDistancia, EstimaTempo -> {V,F}

estimate( elvas, 270, 150 ).
estimate( borba, 250 , 90 ).
estimate( estremoz, 145 , 85 ).
estimate( evora, 95, 68 ).
estimate( montemor, 70, 40 ). 
estimate( vendasnovas, 45, 30 ).
estimate( arraiolos, 190 , 80 ).
estimate( alcacer, 65, 45 ).
estimate( palmela, 40 , 25 ).
estimate( almada, 25, 20 ).
estimate( alandroal, 180, 90 ).
estimate( redondo, 170 , 80 ).
estimate( monsaraz, 120 , 70 ).
estimate( barreiro, 30 , 20 ).
estimate( baixabanheira, 33, 25 ).
estimate( moita, 35, 20 ). 
estimate( alcochete, 26, 15 ).
estimate( lisboa, 0, 0).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado goal: Localidade -> {V,F}

goal( lisboa ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% UTILS

inverse( Xs, Ys ):-
	inverse( Xs, [], Ys ).

inverse( [], Xs, Xs ).
inverse( [X|Xs],Ys, Zs ):-
	inverse( Xs, [X|Ys], Zs ).

select( E, [E|Xs], Xs ).
select( E, [X|Xs], [X|Ys] ) :- select( E, Xs, Ys ).

no( Question ) :-
    Question, !, fail.
no( Question ).

member( X, [X|_] ).
member( X, [_|Xs] ):-
	member( X, Xs ).		

mwrite( [] ).
mwrite( [X|L] ):- write( X ), nl, mwrite( L ).

adjacent_dist( [Nodo|Path]/Cost/_, [Next, Node|Path]/NewCost/EstDist) :- edge( Node, Next, StepCost, _ ),
                                                                         \+ member( Next, Path ),
                                                                         NewCost is Cost + StepCost,
                                                                         estimate( Next, EstDist, _ ).

adjacent_time( [Node|Path]/Cost/_, [Next,Node|Path]/NewCost/EstTime ) :- edge(Node, Next, _, StepTime),
	                                                                     \+ member(Next, Caminho),
	                                                                     NewCost is Custo + StepTime,
	                                                                     estimate(Next, _ , EstTime).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% STAR SEARCH

solve_star( Node,PathDist/CostDist, PathTime/CostTime ) :- estimate( Node, EstD, EstT ),
                                                           star_dist( [[Node]/0/EstD], InPath/CostDist/_ ),
	                                                       star_time( [[Node]/0/EstT], InvPathTime/CostTime/_ ),
	                                                       inverse( InPath, PathDist ),
	                                                       inverse( InvPathTime, PathTime ).

%   - distance

get_best_dist( [Path], Path ) :- !.
get_best_dist( [Path1/Cost1/Est1, _/Cost2/Est2|Paths], BestPath ) :- Cost1 + Est1 =< Cost2 + Est2
                                                                    , !
                                                                    , get_best_dist( [Path1/Cost1/Est1|Path], BestPath ).
get_best_dist([_|Paths], BestPath) :- get_best_dist( Paths, BestPath ).

expand_star_dist( Path, ExPath ) :- findall( New, adjacent_dist( Path, New ), ExPath ).

star_dist( Paths, Path ) :- get_best_dist( Paths, Path ),
                            Path = [Node|_]/_/_, 
                            goal( Node ).

star_dist( Paths, Sol ) :- get_best_dist( Paths, BestPath ), 
                           select( BestPath, Paths, Other ),
                           expand_star_dist( BestPath, ExPath ),
                           append( Other, ExPath, News ),
                           star_dist( News, Sol ).

%   - time

get_best_time( [Path], Path ) :- !.
get_best_time( [Path1/Cost1/Est1,_/Cost2/Est2|Paths], BestPath ) :- Cost1 + Est1 =< Cost2 + Est2
                                                                  , !
                                                                  , get_best_time( [Path1/Cost1/Est1|Paths], BestPath ). 
get_best_time( [_|Paths], BestPath ) :- get_best_time( Paths, BestPath ).

expand_star_time( Path, ExPath ) :- findall( New, adjacent_time( Path, New ), ExPath ).

star_time( Paths, Path ) :- get_best_time( Paths, Path ),
	                        Path = [Node|_]/_/_,
	                        goal( Node ).

star_time( Paths, Sol ) :- get_best_time( Paths, BestPath ),
	                       select( BestPath, Paths, Other ),
                           expand_star_time( BestPath, ExPath ),
	                       append( Other, ExPath, News ),
                           star_time( News, Sol ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% GREED SEARCH

solve_greedG( Node, PathDist/CostDist, PathTime/CostTime ) :- estimate( Node, EstD, EstI ),
	                                                          greed_distG( [[Node]/0/EstD], InPath/CostDist/_ ),
	                                                          greed_timeG( [[Node]/0/EstI], InvPathTime/CostTime/_ ),
	                                                          inverse( InPath, PathDist ),
	                                                          inverse( InvPathTime, CostTime ).

%   - distance

get_best_distG( [Path], Path ) :- !.
get_best_distG( [Path1/Cost1/Est1, _/Cost2/Est2|Paths], BestPath ) :- Est1 =< Est2
                                                                      , !
                                                                      , get_best_distG( [Path1/Cost1/Est1|Paths], BestPath ). 
get_best_distG( [_|Paths], BestPath ) :- get_best_distG( Paths, BestPath ).

expand_greed_distG( Path, ExPath ) :- findall( New, adjacent_dist( Path, New ), ExPath ).

greed_distG( Paths, Path ) :- get_best_distG( Paths, Path ),
	                                Path = [Node|_]/_/_,
	                                goal( Node ).

greed_distG( Paths, Sol ) :- get_best_distG( Paths, BestPath ),
	                         select( BestPath, Paths, Others ),
	                         expand_greed_distG( BestPath, ExPath ),
	                         append( Others, ExPath, NewPaths ),
                             greed_distG( NewPaths, Sol ).	

%   - time

get_best_timeG( [Path], Path ) :- !.
get_best_timeG( [Path1/Cost1/Est1,_/Cost2/Est2|Paths], BestPath) :- Est1 =< Est2
                                                                    , !
                                                                    , get_best_timeG([Path1/Cost1/Est1|Paths], BestPath). 
get_best_timeG([_|Paths], BestPath) :- get_best_timeG(Paths, BestPath).

expand_greed_timeG( Path, ExPath ) :- findall( New, adjacent_time( Path, New ), ExPath ).

greed_timeG( Paths, Path ) :- get_best_timeG( Paths, Path ),
	                        Path = [Nodo|_]/_/_,
	                        goal( Node ).

greed_timeG( Paths, Sol ) :- get_best_timeG( Paths, BestPath ),
	                         select(BestPath, Paths, Other),
	                         expand_greed_timeG(BestPath, ExPath),
	                         append(OutrosPaths, ExPath, NewPaths),
                             greed_timeG(NewPaths, Sol).