:- consult([transportes,estafetas,clientes,encomendas]).
:- op(900,xfy,'::').
:-style_check(-discontiguous).
:-style_check(-singleton).

% ------------- ESTAFETA -------------- --- -- -- - - - - 

% Não permitir a INSERÇÃO de conhecimento repetido pelo id 
+estafeta( IdE, _, _ ) :: (
                            solucoes( A, estafeta( IdE, A, _ ), R ),
                            comprimento( R, 1)
                          ).

% Não permitir REMOVER um IDEstafeta já associado a uma encomenda
-estafeta( IdE, _, _ ) :: (
                            solucoes( A, encomenda( A, ( IdE, _, _ ), _, _, _), R )
		    ).

% ------------- CLIENTE ---------------- --- -- -- - - - - 

% Não permitir a INSERÇÃO de conhecimento repetido pelo id 
+cliente( IdC, _, _ ) :: (
                            solucoes( A, cliente( IdC, A, _ ), R ),
                            comprimento( R, 1 )
                         ).

% Não permitir REMOVER um IDCliente já associado a uma encomenda
-cliente( IdC, _, _ ) :: (
                            solucoes( A, encomenda( A, ( _, _, IdC ), _, _, _), R )
		    ).

% ------------- ENCOMENDA -------------- --- -- -- - - - - 

% Não permitir a INSERÇÃO de conhecimento repetido pelo id 
+encomenda( Id, _, _, _, _ ) :: (
                                    solucoes( ID, encomenda( Id, _, _, _, ID ), R ),
                                    comprimento( R, 1 )
                                ).
