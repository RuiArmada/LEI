%Ficheiro de exemplo da base de conhecimento, verdadeiro ficheiro tem o nome "knowledge.pl"

%estafeta(IdEstafeta, Nome, [(IdEncomenda, Classificacao)])
estafeta(1, 'Ana Caxo Paulo', [(1,2),(3,4),(4,3)]).
estafeta(2, 'Ramiro Silva', [(2,1),(5,2)]).
estafeta(3, 'Mike Oxlong', [(0,4),(6,5),(7,4)]).


%cliente(IdCliente, Nome,  [IdEncomenda])
cliente(1, 'Marco e Gina', [1,2,4]).
cliente(2, 'Mia Khalifa', [0,3,6]).
cliente(3, 'Ermelinda Ribeiro',[5,7]).


%encomenda(IdEncomenda, (IdEstafeta, idTransporte, idcliente), (Data, prazoEntrega), (Peso, Volume, Valor, Estado), (codigo, postal))
%estado : entregue | pendente | cancelada

encomenda(0, (3,2,2), (19/01/2021,72), (16,2,25,pendente), (4700,691)).
encomenda(1, (1,3,1), (14/04/2019,41), (85.87,75,48,pendente),(1234,214)).
encomenda(2, (2,1,1), (10/08/2021,64), (4.13,85,509,entregue),(4672,451)).
encomenda(3, (1,1,2), (23/11/2020,21), (3.0,3,147,entregue),(4601,561)).
encomenda(4, (1,2,1), (27/07/2019,56), (19.25,32,355,entregue),(8214,823)).
encomenda(5, (2,1,3), (08/05/2021,44), (2,33,612,cancelada),(0502,145)).
encomenda(6, (3,3,2), (19/01/2021,21), (98.92,45,125,perdida),(5150,142)).
encomenda(7, (3,2,3), (26/04/2021,14), (14.14,48,342,cancelada),(4444,666)).


%transporte(idTransporte,Nome, PesoMaximo, VelocidadeMedia, nivelEcologico)
transporte(1, bicicleta, 5, 10, 3).
transporte(2, mota, 20, 35, 2).
transporte(3, carro, 100, 25, 1).
