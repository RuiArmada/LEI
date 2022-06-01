:- consult([transportes,clientes,estafetas,encomendas,utils]).
:- dynamic checkState/3, checkTransport/4, checkValidade/4, max_on_snd/2, freqs/2, sumTuples/3, encomenda/5, estafeta/3, transporte/5, cliente/3, evolucao/1, involucao/1.

%============================================================================================
%                                       ADD/REMOVE

add_estafeta(Nome) :-
	findall(I,estafeta(I,_,_),Y),
	max_list(Y,Id),
	IdE is Id + 1,
	evolucao(estafeta(IdE,Nome,[])).

rem_estafeta(IdE) :-
	estafeta(IdE,_,IdEnc),
	maplist(arg(1),IdEnc,Encomendas),
	maplist(atualizacao_encomenda(0),Encomendas),
	involucao(estafeta(IdE,_,_)).

add_cliente(Nome) :- 
	findall(I,cliente(I,_,_),Y),
	max_list(Y,Id),
	IdC is Id + 1,
	evolucao(cliente(IdC,Nome,[])).

rem_cliente(IdC) :-
	cliente(IdC,_,Encomendas),
	maplist(rem_encomenda,Encomendas),
	involucao(cliente(IdC,_,_)).
	
add_encomenda(IdE,IdC,Data,Prazo,Peso,Volume,Valor,Codigo,Postal) :-
	findall(I,encomenda(I,_,_,_,_),Y),
	estafeta(IdE,_,Enco),
	cliente(IdC,_,Clie),
	max_list(Y,Id),
	IdEnc is Id + 1,
	append(Clie,[IdEnc],EncCli),
	append(Enco,[(IdEnc,2)],EncEst),
	(5=<Peso -> IdT is 1; (20=< Peso -> IdT is 2 ; IdT is 3)),
		evolucao(encomenda(IdEnc,(IdE,IdT,IdC),(Data,Prazo),(Peso,Volume,Valor,pendente),(Codigo,Postal))),
		atualizacao_c(IdC,EncCli),
		atualizacao_e(IdE,EncEst).

rem_encomenda(IdEnc) :-
	encomenda(IdEnc,(IdE,A,IdC),B,C,D),
	estafeta(IdE,_,Scores),
	cliente(IdC,_,Encomendas),
	delete((IdEnc,_),Scores,ScoresU),
	delete(IdEnc,Encomendas,EncomendasU),
	atualizacao_c(IdC,EncomendasU),
	atualizacao_e(IdE,ScoresU),
	involucao(encomenda(IdEnc,(IdE,A,IdC),B,C,D)).

%============================================================================================
%                                       PREDICADOS

% predicado 1
ecotrans(Res) :- 
    transporte(IdBike, bicicleta, _, _, _),
    findall((Id, N)
        , (estafeta(Id, _, _)
            , findall(_, encomenda(_, (Id, IdBike, _), _, _, _), Es)
            , length(Es, N))
        , L),
    max_on_snd(L, (Res, _)).

% predicado 2
estcliente(IdCliente, Res) :-
    findall((IdEst,IdEnc), encomenda(IdEnc,(IdEst,_,IdCliente), _, _, _), Res).

% predicado 3
servEstaf(IdEst, Res) :- 
    findall(IdCliente, encomenda(_,(IdEst, _, IdCliente), _, _,_), Res).

% predicado 4
%encomenda(IdEncomenda, (IdEstafeta, idTransporte, idcliente), (Data, prazoEntrega), (Peso, Volume, Valor, Estado), concelho)
valordia(Data, TotalFatorado) :-
    findall(Valor, encomenda(_, (_,_,_), (Data, _), (_,_,Valor,_), _), Values),
    sum_list(Values, TotalFatorado).

% predicado 5
freqZona(Res):-
	findall((Concelho),encomenda(_,_,_,(_,_,_,entregue),(Concelho,_)),L),
	freqs(L,Res).

% predicado 6
classificacaoMedia( IdEst, Media ):-                                
        estafeta(IdEst, _, L),
        sumTuples(2,L, Sum ),                      
        length( L, Length ),
        (Length > 0 -> Media is div(Sum,Length) ; Media is 0).

% predicado 7
entregasTransporte(Di/Mi/Yi,Df/Mf/Yf,B,M,C):-
	findall((D/M/Y,IdTrans),encomenda(_,(_,IdTrans,_),(D/M/Y,_),(_,_,_,entregue),_),L),
	checkValidade(L,Di/Mi/Yi,Df/Mf/Yf,T),
	checkTransport(T,B,M,C).

%predicado 8
entregasEstafeta(Di/Mi/Yi,Df/Mf/Yf,Out):-
	findall((D/M/Y,IdEst),encomenda(_,(IdEst,_,_),(D/M/Y,_),(_,_,_,entregue),_),Estafetas),
	checkValidade(Estafetas,Di/Mi/Yi,Df/Mf/Yf,T),
	freqs(T,Out).

% predicado 9
estadoIntrevalo(Di/Mi/Yi,Df/Mf/Yf,E,N):-
	findall((D/M/Y,Estado),encomenda(_,_,(D/M/Y,_),(_,_,_,Estado),_),L),
	checkValidade(L,Di/Mi/Yi,Df/Mf/Yf,Estados),
	checkState(Estados,E,N).

% predicado 10
pesototal(IdEst, Data, Res) :-
    findall(Peso, encomenda(_,(IdEst,_,_),(Data,_),(Peso, _, _, _), _), Pesos),
    sum_list(Pesos, Res).

