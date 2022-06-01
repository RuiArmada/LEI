:- consult([transportes,clientes,estafetas,encomendas,arestas]).
:- dynamic aresta/3.

%coisas do predicado 4
calculaVelF(Kgs,(bicicleta,Vf)) :-
    Kgs<5,
    Vf is 10-(Kgs*0.7).
calculaVelF(Kgs,(mota,Vf)) :- 
    Kgs<20,
    Vf is 35-(Kgs*0.5).
calculaVelF(Kgs,(carro,Vf)) :-
    Kgs<100,
    Vf is 25-(Kgs*0.1).

min_on_snd([H], H).
min_on_snd([(A0/N0)|T], Res) :-
    min_on_snd(T, (A1/N1)),
    Min is min(N0, N1),
    (Min =:= N0 -> Res = (A0/N0); Res = (A1/N1)).
%fim de coisas do predicado 4

%STUFF
escrever([]).
escrever([H|T]) :-
	write(H), write("\n"),
	escrever(T).

% nao( Questao ) :-
%     Questao, !, fail.
% nao( Questao ).
% 
% 
% appendToList([],L,L).
% appendToList([H|T],L,[H|Z]):- appendToList(T,L,Z).
% appendToListS([],L).
% appendToListS(R,[R|L]).

inverteLista([],Z,Z).
inverteLista([H|T],Z,Acc) :- inverteLista(T,Z,[H|Acc]).


inverso(Xs, Ys):-
	inverso(Xs, [], Ys).

inverso([], Xs, Xs).
inverso([X|Xs],Ys, Zs):-
	inverso(Xs, [X|Ys], Zs).

removeEmptyL([[]|Ls], Res) :- removeEmptyL(Ls,Res).
removeEmptyL([L|Ls],Res) :- 
    removeEmptyL(Ls,Res2),
    append(L,Res2,Res).

concatenar([],[]).
concatenar([[],X|Ls],Res) :- concatenar([X|Ls],Res).
concatenar([X,[]|Ls],Res) :- concatenar([X|Ls],Res).
concatenar([L1,L2|Ls],Res) :-
    concat(L1,L2,L3),
    concatenar([L3|Ls],C),
    C is Res.

%____
%Print
q3Print([]).
q3Print([X|L]):-
	(A,B,C,D) = X,
		format('{  Caminho: ~w,\n\t Numero de Encomendas: ~d,\n\t Peso Total: ~2f,\n\t Volume Total: ~2f  };\n',[A,B,C,D]),
		q3Print(L).
