:- consult([transportes,estafetas,clientes,encomendas,invariantes]).
:- op(900,xfy,'::').
:-dynamic '-'/1.

%============================================================================================
%                                       Auxiliar

% [(A, Number)] -> (A, Number)
max_on_snd([H], H).
max_on_snd([(A0,N0)|T], Res) :-
    max_on_snd(T, (A1,N1)),
    Max is max(N0, N1),
    (Max =:= N0 -> Res = (A0,N0); Res = (A1,N1)).

%[X]->[(X,n)]
freqs([],[]).
freqs(In,Out):-
    freqs(In,[],Out).

freqs([],Out,Out).
freqs([X|Xs],Table,Out):-
    \+ member((X,_),Table),
    freqs(Xs,[(X,1)|Table],Out).

freqs([X|Xs],Table,Out) :-
    selectchk((X,N),Table, Others),
    M is N + 1,
    freqs(Xs, [(X,M)| Others], Out).

% [(tuples)] -> Sum(values in pos X)
sumTuples(_,[],0).
sumTuples(Pos,[Tuple|T],Sum) :-
        sumTuples(Pos,T,Sum1),
        arg(Pos,Tuple,Elem),
        Sum is Sum1 + Elem.
%Concat
concat([],L2,L2). concat([Head|Tail],L2,[Head|L3]) :- concat(Tail,L2,L3).

% (D_inicial/Mi/Yi,D_final/Mf/Yf,[Sent],[NotSent],(D/M/Y,Estado)) -> [Lista]
checkValidade([],_,_,[]).
checkValidade([(D/M/Y,Estado)|T],Di/Mi/Yi,Df/Mf/Yf,Out):-
	checkValidade(T,Di/Mi/Yi,Df/Mf/Yf,Out1),
	(Y<2022, Y>1900, M<13, M>0,D<32, D>0->
		(((Yi<Y, Y<Yf, Mi=<M, M=<Mf);(Y==Yi, Mi=<M);(Y==Yf, M=<Mf);(Y==Yi, Mi==M, Di=<D);(Y==Yf, M==Mf, D=<Df))-> concat([Estado],Out1,Out);concat([],Out1,Out))).

% [Estados] -> XEntregues Y!Entregues
checkState([],0,0).
checkState([Estado|T],E,N):-
	checkState(T,E1,N1),
	(Estado = entregue-> (E is E1 + 1,N is N1); (E is E1, N is N1 + 1)).

% [Transporte] -> XEntregues YEntregues ZEntregues
checkTransport([],0,0,0).
checkTransport([IdTrans|T],B,M,C):-
	checkTransport(T,B1,M1,C1),
	(IdTrans = 1 -> B is B1 + 1,M is M1, C is C1;(IdTrans = 2 -> M is M1 + 1, B is B1, C is C1;C is C1 + 1, B is B1, M is M1)).

% remove Elemento de Lista
delete(_, [], []):- !.
delete(X, [X|L1], L2):- !, delete(X,L1,L2).
delete(X, [H|L1], [H|L2]):- !,delete(X,L1,L2).

find(_, [], []):- !.
find(X, [X|L1], L2):- !, find(X,L1,L3), append(L3,[X],L2).
find(_, L1, L):- !,find(_,L1,L).


% Print Knowledge base to files (Overwrite them)

printEstafetasFormated:-    
        findall((A,B,C),estafeta(A,B,C),Y),    
        maplist(format('estafeta(~q).\n'),Y).    
     
printClientesFormated:-    
        findall((A,B,C),cliente(A,B,C),Y),    
        maplist(format('cliente(~q).\n'),Y).
        
printEncomendaFormated:-    
        forall(encomenda(A,(B,C,D),(E,F),(G,H,I,J),(K,L)),format('encomenda(~d,(~d,~d,~d),(~w,~d),(~2f,~2f,~d,~w),(~d,~d)).\n',[A,B,C,D,E,F,G,H,I,J,K,L])).


printTransportesFormated:-    
        findall((A,B,C,D,E),transporte(A,B,C,D,E),Y),    
        maplist(format('transporte(~q).\n'),Y).    
     
saveConhecimento:-    
        (tell('transportes.pl'),write('%transporte(idTransporte,nome, pesoMáximo, velocidadeMédia, nivelEcológico).\n'), printTransportesFormated), told,    
        (tell('clientes.pl'),write('%cliente(idCliente,nome,[encomendas]).\n'), printClientesFormated), told,    
        (tell('estafetas.pl'),write('%estafeta(idEstafeta, nome, [(idEncomenda, classificação)]).\n'), printEstafetasFormated), told,    
        (tell('encomendas.pl'),write('%encomenda(idEncomenda, (idEstafeta, idTransporte, idcliente), (data, prazoEntrega), (peso, volume, valor, estado), (codigo, postal)).\n'), printEncomendaFormated), told.

atualizacao_c(Id,L):-
	cliente(Id,Nome,_),
	involucao(cliente(Id,Nome,_)),
	evolucao(cliente(Id,Nome,L)).

atualizacao_e(Id,L):-
	estafeta(Id,Nome,_),
	involucao(estafeta(Id,Nome,_)),
	evolucao(estafeta(Id,Nome,L)).

atualizacao_encomenda(Id,IdE):-
	encomenda(IdE,(IdEst,IdT,IdC),D,E,P),
	remocao(encomenda(IdE,(IdEst,IdT,IdC),D,E,P)),
	insercao(encomenda(IdE,(Id,IdT,IdC),D,E,P)).

% INVARIANTES

% Evolucao
teste([]).
teste([X|LR]):-X,teste(LR).

evolucao(Termo) :- 
    solucoes(Invariante,+Termo::Invariante, Lista),
    insercao(Termo),
    teste(Lista).

insercao(Termo) :- assert(Termo).
insercao(Termo) :- retract(Termo),!,fail.

% Involucao
involucao(Termo) :- Termo,
                    solucoes(Invariante,-Termo::Invariante,Lista),
                    remocao(Termo),
                    teste(Lista).

remocao(Termo) :- retract(Termo).
remocao(Termo) :- assertz(Termo),!,fail.

solucoes(X,Y,Z) :- findall(X,Y,Z).

comprimento(S,N) :- length(S,N).
