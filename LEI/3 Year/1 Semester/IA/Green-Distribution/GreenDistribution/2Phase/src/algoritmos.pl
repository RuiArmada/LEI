:- consult([nodos,encomendas,estimativas,arestas,graph]).

hasNext(Nodo,Adjs) :-     
    findall(Adj,    
           (aresta(Nodo,Adj,_);    
           aresta(Adj,Nodo,_)),    
           Adjs).

addDest(Node):-
	insercao(destino(Node)).

remDest(Node):-
	remocao(destino(Node)).

insercao(Termo) :- assertz(Termo).
insercao(Termo) :- retract(Termo),!,fail.

remocao(Termo) :- retract(Termo).
remocao(Termo) :- assertz(Termo),!,fail.

origem(0).

list_tail([_|Xs],Xs).

adjacent(X,Y,C):- aresta(X,Y,C).
adjacent(X,Y,C):- aresta(Y,X,C).

move_cyclefree(Visited, Node, NextNode, Cost) :-
	adjacent(Node, NextNode, Cost),
	\+ member(NextNode, Visited).

move([Node|Path]/Cost/_, [NextNode,Node|Path]/NewCost/Est) :-
	adjacent(Node, NextNode, StepCost),
	\+ member(NextNode, Path),
	NewCost is Cost + StepCost,
	estimativa(NextNode, Est).

merge_list([],L,L ).
merge_list([H|T],L,[H|M]):-
    merge_list(T,L,M).

%------- Query 3 ------------

findInfoEnc(IdEnc,Weight, Volume) :-
	encomenda(IdEnc,_,_,(Weight,Volume,_,_),_).

move_cyclefree_NWV(Visited, Node, NextNode, (NumEncs, Weight, Volume)) :-
	adjacent(Node, NextNode, _),
	\+ member(NextNode, Visited),
	node(Node, L),
	length(L, NumEncs),
	maplist(findInfoEnc, L,LW,LV),
	sumlist(LW,Weight),
	sumlist(LV,Volume).

%---------------------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Inefficient Search%

caminhoT(O,Node,Cam,Cost):-
	caminho(O,Node,P,C),
	reverse(P,Back),
	list_tail(Back,PBack),
	append(P,PBack,Cam),
	Cost is C*2.

caminho(A,B,P,C):- caminho1(A,[B],P,C).
caminho1(A,[A|P1],[A|P1],0).
caminho1(A,[Y|P1],P,C):- 
	adjacent(X,Y,C1), 
    not(member(X,[Y|P1])), 
	caminho1(A,[X,Y|P1],P,C2),
	C is 0+C1+C2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%DepthFirst Search%

solve_depthfirst_cyclefree(Path,Cost) :-
	origem(Node),
	depthfirst_cyclefree([Node], Node, RevPath, C),
	reverse(RevPath, PathT),
	list_tail(RevPath,Back),
	append(PathT,Back,Path),
	Cost is C*2.

depthfirst_cyclefree(Visited, Node, Visited, 0) :-
	destino(Node).

depthfirst_cyclefree(Visited, Node, Path, Cost) :-
	move_cyclefree(Visited, Node, NextNode, C1),
	depthfirst_cyclefree([NextNode|Visited], NextNode, Path, C2),
	Cost is C1+C2.

%-------- Query 3--------------------

solve_depthfirst_cyclefree_NWV(Path,(NumEncs, Weight, Volume)) :-
	origem(Node),
	depthfirst_cyclefree_NWV([Node], Node, RevPath, (NumEncs, Weight, Volume)),
	reverse(RevPath, PathT),
	list_tail(RevPath,Back),
	append(PathT,Back,Path).

depthfirst_cyclefree_NWV(Visited, Node, Visited, (NumEncs,0,0)) :-
	destino(Node),
	node(Node,L),
	length(L, NumEncs).
	
depthfirst_cyclefree_NWV(Visited, Node, Path, (NumEncs, Weight, Volume)) :-
	move_cyclefree_NWV(Visited, Node, NextNode, (NumEncs1, W1, V1)),
	depthfirst_cyclefree_NWV([NextNode|Visited], NextNode, Path, (NumEncs2, W2, V2)),
	NumEncs is NumEncs1+NumEncs2,
	Weight is W1+W2,
	Volume is V1+V2.

%-------------------------------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Iterative Deepening Search%

reversP(O,Node,Path,Cost):-
	path(O, Node, RevPath,C),
	reverse(RevPath, Cam),
	list_tail(RevPath,Back),
	append(Cam,Back,Path),
	Cost is C*2.


path(Node, Node, [Node],0).
path(FirstNode, LastNode, [LastNode|Path],Cost) :-
	path(FirstNode, PenultimateNode, Path, C2),
	move_cyclefree(Path, PenultimateNode, LastNode, C1),
	Cost is C1+C2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Astar Search%

solve_astar(Nodo,Caminho/CustoDist) :-
	estimativa(Nodo,EstimaD),
	aStar([[Nodo]/0/EstimaD],InvCaminho/CD/_),
	reverse(InvCaminho,CaminhoDistancia),
	list_tail(InvCaminho,BackCaminho),
	append(CaminhoDistancia,BackCaminho,Caminho),
	CustoDist is CD*2.

aStar(Caminhos,Caminho) :-
	getBest(Caminhos,Caminho),
	Caminho = [Nodo|_]/_/_,destino(Nodo).

aStar(Caminhos,SolucaoCaminho) :-
	getBest(Caminhos,MelhorCaminho),
	select(MelhorCaminho,Caminhos,OutrosCaminhos),
	expandAstar(MelhorCaminho,ExpCaminhos),
	append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
        aStar(NovoCaminhos,SolucaoCaminho).	

getBest([Caminho],Caminho) :- !.
getBest([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos],MelhorCaminho) :-
	Custo1 + Est1 =< Custo2 + Est2,!,
	getBest([Caminho1/Custo1/Est1|Caminhos],MelhorCaminho). 
getBest([_|Caminhos],MelhorCaminho) :- 
	getBest(Caminhos,MelhorCaminho).


expandAstar(Path, ExpPaths) :-
	findall(NewPath, move(Path,NewPath), ExpPaths).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Greedy Search%

solve_greedy(Nodo,Path/Cost) :-
	estimativa(Nodo,EstimaD),
	greedy([[Nodo]/0/EstimaD],InvCaminho/CustoDist/_),
	reverse(InvCaminho,CaminhoDistancia),
	list_tail(InvCaminho,Back),
	append(CaminhoDistancia,Back,Path),
	Cost is CustoDist*2.

greedy(Caminhos,Caminho) :-
	getBestGreedy(Caminhos,Caminho),
	Caminho = [Nodo|_]/_/_,
	destino(Nodo).

greedy(Caminhos,SolucaoCaminho) :-
	getBestGreedy(Caminhos,MelhorCaminho),
	select(MelhorCaminho,Caminhos,OutrosCaminhos),
	expandGreedy(MelhorCaminho,ExpCaminhos),
	append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
        greedy(NovoCaminhos,SolucaoCaminho).	

getBestGreedy([Caminho],Caminho) :- !.
getBestGreedy([Caminho1/Custo1/Est1,_/_/Est2|Caminhos],MelhorCaminho) :-
	Est1 =< Est2,!,
	getBestGreedy([Caminho1/Custo1/Est1|Caminhos],MelhorCaminho). 
getBestGreedy([_|Caminhos],MelhorCaminho) :- 
	getBestGreedy(Caminhos,MelhorCaminho).
	

expandGreedy(Caminho,ExpCaminhos) :-
	findall(NovoCaminho,move(Caminho,NovoCaminho),ExpCaminhos).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%%Search Algorithms

%Inefficient Search%

% Argumentos
% Node -> Onde quero ir.
% Cam -> Caminho de 0 ate Node
% Cost -> Custo de 0 ate Node.
solveInefeciente(Node,Cam,Cost):-
	addDest(Node),
	origem(G),
	caminhoT(G,Node,Cam,Cost),
	remDest(Node).

solveAllInefeciente(Node,L):-
	addDest(Node),
	origem(G),
	findall((P,C),caminhoT(G,Node,P,C),L),
	remDest(Node).

%Iterative Deepening Search%

% Argumentos
% Node -> Onde quero ir.
% Cam -> Caminho de 0 ate Node
% Cost -> Custo de 0 ate Node.
%
% Observacoes, Nao e possivel fazer findall porque entra em loop infinito pois a solucao e sempre a mesma.
solveIterativeDeepening(Node,Cam,Cost) :-
	addDest(Node),
	origem(O),
	reversP(O,Node,Cam,Cost),
	remDest(Node).

solveAllIterativeDeepening(Node,L) :-
	addDest(Node),
	origem(O),
	findall((P,C),reversP(O,Node,P,C),L),
	remDest(Node).

%DepthFirst Search%

% Argumentos
% Node -> Onde quero ir.
% Path -> Caminho de 0 ate Node.
% Cost -> Custo de 0 ate Node
solveDepthFirst(Node,Path,Cost):-
	addDest(Node),
	solve_depthfirst_cyclefree(Path,Cost),
	remDest(Node).

solveAllDepthFirst(Node,L):-
	addDest(Node),
	findall((Path,Cost),solve_depthfirst_cyclefree(Path,Cost),L),
	remDest(Node).


%Astar Search%

%Argumentos
%Node -> Onde quero ir.
%Cam -> Caminho/Custo de 0 ate Node.
solveAstar(Node,Cam):-
	addDest(Node),
	origem(NO),
	solve_astar(NO,Cam).
solveAllAstar(Node,L):-
	addDest(Node),
	origem(NO),
	findall(Cam,solve_astar(NO,Cam),L),
	remDest(Node).

%Greedy Search%

%Argumentos
%Node -> Onde quero ir.
%Cam -> Caminho/Custo de 0 ate Node.
solveGreedy(Node,Cam):-
	addDest(Node),
	origem(NO),
	solve_greedy(NO,Cam),
	remDest(Node).

solveAllGreedy(Node,L):-
	addDest(Node),
	origem(NO),
	findall(Cam,solve_greedy(NO,Cam),L),
	remDest(Node).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SolveAll Algorithm
% Merge All outputs to a list

solveAll(Node,[]):-
	hasNext(Node,E),
	length(E,A),
	(A=0).
solveAll(Node,L):-
	hasNext(Node,E),
	length(E,A),A>0,
	addDest(Node),
	origem(O),
	findall((P/C),caminhoT(O,Node,P,C),IN),
	reversP(O,Node,P1,C1),
	ID = [P1/C1],
	findall((P2/C2),solve_depthfirst_cyclefree(P2,C2),DF),
	findall(Cam1,solve_astar(O,Cam1),AS),
	findall(Cam2,solve_greedy(O,Cam2),GD),
	merge_list(IN,ID,O1),
	merge_list(DF,AS,O2),
	merge_list(O1,GD,O3),
	merge_list(O2,O3,O4),
	sort(O4,L),
	remDest(Node).

%--------------------------------- - - - - - - -- -  -- - - - - - - - - -  
% QuickSort Algorithm

quicksort([X|Xs],Ys) :-
	(_,Num,_,_) = X,
	partition(Xs,Num,Left,Right),
	quicksort(Left,Ls),
	quicksort(Right,Rs),
	append(Ls,[X|Rs],Ys).
quicksort([],[]).
  
partition([X|Xs],Y,[X|Ls],Rs) :-
	(_,Num,_,_) = X,
	Num >= Y,
	partition(Xs,Y,Ls,Rs).
partition([X|Xs],Y,Ls,[X|Rs]) :-
	(_,Num,_,_) = X,
	Num < Y, 
	partition(Xs,Y,Ls,Rs).
partition([],_,[],[]).
  
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SolveAll Algorithm
% Merge All outputs to a list

solveAllDepthFirst_NwV1(Node,L):-
	addDest(Node),                                                        
	findall((Path,N,W,V),solve_depthfirst_cyclefree_NWV(Path,(N,W,V)),L),                                            
	remDest(Node).                                                        
											   
solveAllDepthFirst_NwV(Res):-
	findall(N,node(N,_),Ln),
	maplist(solveAllDepthFirst_NwV1,Ln,Res1),
	append(Res1,Res2),
	quicksort(Res2,Res).

