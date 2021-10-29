\documentclass[a4paper]{article}
\usepackage[a4paper,left=3cm,right=2cm,top=2.5cm,bottom=2.5cm]{geometry}
\usepackage{palatino}
\usepackage[colorlinks=true,linkcolor=blue,citecolor=blue]{hyperref}
\usepackage{graphicx}
\usepackage{cp2021t}
\usepackage{subcaption}
\usepackage{adjustbox}
\usepackage{color}
\definecolor{red}{RGB}{255,  0,  0}
\definecolor{blue}{RGB}{0,0,255}
\def\red{\color{red}}
\def\blue{\color{blue}}
%================= local x=====================================================%
\def\getGif#1{\includegraphics[width=0.3\textwidth]{cp2021t_media/#1.png}}
\let\uk=\emph
\def\aspas#1{``#1"}
%================= lhs2tex=====================================================%
%include polycode.fmt 
%format (div (x)(y)) = x "\div " y
%format succ = "\succ "
%format ==> = "\Longrightarrow "
%format map = "\map "
%format length = "\length "
%format fst = "\p1"
%format p1  = "\p1"
%format snd = "\p2"
%format p2  = "\p2"
%format Left = "i_1"
%format Right = "i_2"
%format i1 = "i_1"
%format i2 = "i_2"
%format >< = "\times"
%format >|<  = "\bowtie "
%format |-> = "\mapsto"
%format . = "\comp "
%format .=?=. = "\mathbin{\stackrel{\mathrm{?}}{=}}"
%format (kcomp (f)(g)) = f "\kcomp " g
%format -|- = "+"
%format conc = "\mathsf{conc}"
%format summation = "{\sum}"
%format (either (a) (b)) = "\alt{" a "}{" b "}"
%format (frac (a) (b)) = "\frac{" a "}{" b "}"
%format (uncurry f) = "\uncurry{" f "}"
%format (const f) = "\underline{" f "}"
%format TLTree = "\mathsf{TLTree}"
%format (lcbr (x)(y)) = "\begin{lcbr}" x "\\" y "\end{lcbr}"
%format (split (x) (y)) = "\conj{" x "}{" y "}"
%format (for (f) (i)) = "\for{" f "}\ {" i "}"
%format B_tree = "\mathsf{B}\mbox{-}\mathsf{tree} "
\def\ana#1{\mathopen{[\!(}#1\mathclose{)\!]}}
%format <$> = "\mathbin{\mathopen{\langle}\$\mathclose{\rangle}}"
%format (cataA (f) (g)) = "\cata{" f "~" g "}_A"
%format (anaA (f) (g)) = "\ana{" f "~" g "}_A"
%format (cataB (f) (g)) = "\cata{" f "~" g "}_B"
%format (cata (f)) = "\cata{" f "}"
%format (anaB (f) (g)) = "\ana{" f "~" g "}_B"
%format Either a b = a "+" b 
%format fmap = "\mathsf{fmap}"
%format NA   = "\textsc{na}"
%format NB   = "\textsc{nb}"
%format inT = "\mathsf{in}"
%format outT = "\mathsf{out}"
%format Null = "1"
%format (Prod (a) (b)) = a >< b
%format fF = "\fun F "
%format e1 = "e_1 "
%format e2 = "e_2 "
%format Dist = "\fun{Dist}"
%format IO = "\fun{IO}"
%format BTree = "\fun{BTree} "
%format LTree = "\mathsf{LTree}"
%format inNat = "\mathsf{in}"
%format (cataNat (g)) = "\cata{" g "}"
%format Nat0 = "\N_0"
%format Rational = "\Q "
%format toRational = " to_\Q "
%format fromRational = " from_\Q "
%format muB = "\mu "
%format (frac (n)(m)) = "\frac{" n "}{" m "}"
%format (fac (n)) = "{" n "!}"
%format (underbrace (t) (p)) = "\underbrace{" t "}_{" p "}"
%format matrix = "matrix"
%%format (bin (n) (k)) = "\Big(\vcenter{\xymatrix@R=1pt{" n "\\" k "}}\Big)"
%format `ominus` = "\mathbin{\ominus}"
%format % = "\mathbin{/}"
%format <-> = "{\,\leftrightarrow\,}"
%format <|> = "{\,\updownarrow\,}"
%format `minusNat`= "\mathbin{-}"
%format ==> = "\Rightarrow"
%format .==>. = "\Rightarrow"
%format .<==>. = "\Leftrightarrow"
%format .==. = "\equiv"
%format .<=. = "\leq"
%format .&&&. = "\wedge"
%format cdots = "\cdots "
%format pi = "\pi "
%format (curry (f)) = "\overline{" f "}"
%format (cataLTree (x)) = "\llparenthesis\, " x "\,\rrparenthesis"
%format (anaLTree (x)) = "\mathopen{[\!(}" x "\mathclose{)\!]}"
%format delta = "\Delta "

%---------------------------------------------------------------------------

\title{
       	Cálculo de Programas
\\
       	Trabalho Prático
\\
       	MiEI+LCC --- 2020/21
}

\author{
       	\dium
\\
       	Universidade do Minho
}


\date\mydate

\makeindex
\newcommand{\rn}[1]{\textcolor{red}{#1}}
\begin{document}

\maketitle

\begin{center}\large
\begin{tabular}{ll}
\textbf{Grupo} nr. & 28
\\\hline
a93220 & Alexandre Flores	
\\
a93294 & Miguel Gomes	
\\
a90468 & Rui Armada 
\\
a67674 & Tiago Sousa	
\end{tabular}
\end{center}

\section{Preâmbulo}

\CP\ tem como objectivo principal ensinar
a progra\-mação de computadores como uma disciplina científica. Para isso
parte-se de um repertório de \emph{combinadores} que formam uma álgebra da
programação (conjunto de leis universais e seus corolários) e usam-se esses
combinadores para construir programas \emph{composicionalmente}, isto é,
agregando programas já existentes.
  
Na sequência pedagógica dos planos de estudo dos dois cursos que têm
esta disciplina, opta-se pela aplicação deste método à programação
em \Haskell\ (sem prejuízo da sua aplicação a outras linguagens 
funcionais). Assim, o presente trabalho prático coloca os
alunos perante problemas concretos que deverão ser implementados em
\Haskell.  Há ainda um outro objectivo: o de ensinar a documentar
programas, a validá-los e a produzir textos técnico-científicos de
qualidade.

\section{Documentação} Para cumprir de forma integrada os objectivos
enunciados acima vamos recorrer a uma técnica de programa\-ção dita
``\litp{literária}'' \cite{Kn92}, cujo princípio base é o seguinte:
%
\begin{quote}\em Um programa e a sua documentação devem coincidir.
\end{quote}
%
Por outras palavras, o código fonte e a documentação de um
programa deverão estar no mesmo ficheiro.

O ficheiro \texttt{cp2021t.pdf} que está a ler é já um exemplo de
\litp{programação literária}: foi gerado a partir do texto fonte
\texttt{cp2021t.lhs}\footnote{O suffixo `lhs' quer dizer
\emph{\lhaskell{literate Haskell}}.} que encontrará no
\MaterialPedagogico\ desta disciplina descompactando o ficheiro
\texttt{cp2021t.zip} e executando:
\begin{Verbatim}[fontsize=\small]
    $ lhs2TeX cp2021t.lhs > cp2021t.tex
    $ pdflatex cp2021t
\end{Verbatim}
em que \href{https://hackage.haskell.org/package/lhs2tex}{\texttt\LhsToTeX} é
um pre-processador que faz ``pretty printing''
de código Haskell em \Latex\ e que deve desde já instalar executando
\begin{Verbatim}[fontsize=\small]
    $ cabal install lhs2tex --lib
\end{Verbatim}
Por outro lado, o mesmo ficheiro \texttt{cp2021t.lhs} é executável e contém
o ``kit'' básico, escrito em \Haskell, para realizar o trabalho. Basta executar
\begin{Verbatim}[fontsize=\small]
    $ ghci cp2021t.lhs
\end{Verbatim}

%if False
\begin{code}
{-# OPTIONS_GHC -XNPlusKPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, FlexibleInstances #-}
module Main where 
import Cp
import List hiding (fac)
import Nat
import LTree
import Data.List hiding (find)
import Test.QuickCheck hiding ((><),choose,collect)
import qualified Test.QuickCheck as QuickCheck
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad
import Control.Applicative hiding ((<|>))
import System.Process
\end{code}
%endif

\noindent Abra o ficheiro \texttt{cp2021t.lhs} no seu editor de texto preferido
e verifique que assim é: todo o texto que se encontra dentro do ambiente
\begin{quote}\small\tt
\verb!\begin{code}!
\\ ... \\
\verb!\end{code}!
\end{quote}
é seleccionado pelo \GHCi\ para ser executado.

\section{Como realizar o trabalho}
Este trabalho teórico-prático deve ser realizado por grupos de 3 (ou 4) alunos.
Os detalhes da avaliação (datas para submissão do relatório e sua defesa
oral) são os que forem publicados na \cp{página da disciplina} na \emph{internet}.

Recomenda-se uma abordagem participativa dos membros do grupo
de trabalho por forma a poderem responder às questões que serão colocadas
na \emph{defesa oral} do relatório.

Em que consiste, então, o \emph{relatório} a que se refere o parágrafo anterior?
É a edição do texto que está a ser lido, preenchendo o anexo \ref{sec:resolucao}
com as respostas. O relatório deverá conter ainda a identificação dos membros
do grupo de trabalho, no local respectivo da folha de rosto.

Para gerar o PDF integral do relatório deve-se ainda correr os comando seguintes,
que actualizam a bibliografia (com \Bibtex) e o índice remissivo (com \Makeindex),
\begin{Verbatim}[fontsize=\small]
    $ bibtex cp2021t.aux
    $ makeindex cp2021t.idx
\end{Verbatim}
e recompilar o texto como acima se indicou. Dever-se-á ainda instalar o utilitário
\QuickCheck,
que ajuda a validar programas em \Haskell\ e a biblioteca \gloss{Gloss} para
geração de gráficos 2D:
\begin{Verbatim}[fontsize=\small]
    $ cabal install QuickCheck gloss --lib
\end{Verbatim}
Para testar uma propriedade \QuickCheck~|prop|, basta invocá-la com o comando:
\begin{verbatim}
    > quickCheck prop
    +++ OK, passed 100 tests.
\end{verbatim}
Pode-se ainda controlar o número de casos de teste e sua complexidade,
como o seguinte exemplo mostra:
\begin{verbatim}
    > quickCheckWith stdArgs { maxSuccess = 200, maxSize = 10 } prop
    +++ OK, passed 200 tests.
\end{verbatim}
Qualquer programador tem, na vida real, de ler e analisar (muito!) código
escrito por outros. No anexo \ref{sec:codigo} disponibiliza-se algum
código \Haskell\ relativo aos problemas que se seguem. Esse anexo deverá
ser consultado e analisado à medida que isso for necessário.

\subsection{Stack}

O \stack{Stack} é um programa útil para criar, gerir e manter projetos em \Haskell.
Um projeto criado com o Stack possui uma estrutura de pastas muito específica:

\begin{itemize}
\item Os módulos auxiliares encontram-se na pasta \emph{src}.
\item O módulos principal encontra-se na pasta \emph{app}.
\item A lista de depêndencias externas encontra-se no ficheiro \emph{package.yaml}.
\end{itemize}

Pode aceder ao \GHCi\ utilizando o comando:
\begin{verbatim}
stack ghci
\end{verbatim}

Garanta que se encontra na pasta mais externa \textbf{do projeto}.
A primeira vez que correr este comando as depêndencias externas serão instaladas automaticamente.

Para gerar o PDF, garanta que se encontra na diretoria \emph{app}.

\Problema

Os \emph{tipos de dados algébricos} estudados ao longo desta disciplina oferecem
uma grande capacidade expressiva ao programador. Graças à sua flexibilidade,
torna-se trivial implementar \DSL s
e até mesmo \href{http://www.cse.chalmers.se/~ulfn/papers/thesis.pdf}{linguagens de programação}.

Paralelamente, um tópico bastante estudado no âmbito de \DL\ 
é a derivação automática de expressões matemáticas, por exemplo, de derivadas.
Duas técnicas que podem ser utilizadas para o cálculo de derivadas são:

\begin{itemize}
\item \emph{Symbolic differentiation}
\item \emph{Automatic differentiation}
\end{itemize}

\emph{Symbolic differentiation} consiste na aplicação sucessiva de transformações
(leia-se: funções) que sejam congruentes com as regras de derivação. O resultado
final será a expressão da derivada.

O leitor atento poderá notar um problema desta técnica: a expressão
inicial pode crescer de forma descontrolada, levando a um cálculo pouco eficiente.
\emph{Automatic differentiation} tenta resolver este problema,
calculando \textbf{o valor} da derivada da expressão em todos os passos.
Para tal, é necessário calcular o valor da expressão \textbf{e} o valor da sua derivada.

Vamos de seguida definir uma linguagem de expressões matemáticas simples e
implementar as duas técnicas de derivação automática.
Para isso, seja dado o seguinte tipo de dados,

\begin{code}
data ExpAr a = X
           | N a
           | Bin BinOp (ExpAr a) (ExpAr a)
           | Un UnOp (ExpAr a)
           deriving (Eq, Show)
\end{code}

\noindent
onde |BinOp| e |UnOp| representam operações binárias e unárias, respectivamente:

\begin{code}
data BinOp = Sum
           | Product
           deriving (Eq, Show)

data UnOp = Negate
          | E
          deriving (Eq, Show)
\end{code}

\noindent
O construtor |E| simboliza o exponencial de base $e$.

Assim, cada expressão pode ser uma variável, um número, uma operação binária
aplicada às devidas expressões, ou uma operação unária aplicada a uma expressão.
Por exemplo,
\begin{spec}
Bin Sum X (N 10)
\end{spec}
designa |x+10| na notação matemática habitual.

\begin{enumerate}
\item A definição das funções |inExpAr| e |baseExpAr| para este tipo é a seguinte:
\begin{code}
inExpAr = either (const X) num_ops where
  num_ops = either N ops
  ops     = either bin (uncurry Un)
  bin(op, (a, b)) = Bin op a b

baseExpAr f g h j k l z = f -|- (g -|- (h >< (j >< k) -|- l >< z))
\end{code}

  Defina as funções |outExpAr| e |recExpAr|,
  e teste as propriedades que se seguem.
  \begin{propriedade}
    |inExpAr| e |outExpAr| são testemunhas de um isomorfismo,
    isto é,
    |inExpAr . outExpAr = id| e |outExpAr . idExpAr = id|:
\begin{code}
prop_in_out_idExpAr :: (Eq a) => ExpAr a -> Bool
prop_in_out_idExpAr = inExpAr . outExpAr .==. id

prop_out_in_idExpAr :: (Eq a) => OutExpAr a -> Bool
prop_out_in_idExpAr = outExpAr . inExpAr .==. id
\end{code}
    \end{propriedade}

  \item Dada uma expressão aritmética e um escalar para substituir o |X|,
	a função

\begin{quote}
      |eval_exp :: Floating a => a -> (ExpAr a) -> a|
\end{quote}

\noindent calcula o resultado da expressão. Na página \pageref{pg:P1}
    esta função está expressa como um catamorfismo. Defina o respectivo gene
    e, de seguida, teste as propriedades:
    \begin{propriedade}
       A função |eval_exp| respeita os elementos neutros das operações.
\begin{code}
prop_sum_idr :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_sum_idr a exp = eval_exp a exp .=?=. sum_idr where
  sum_idr = eval_exp a (Bin Sum exp (N 0))

prop_sum_idl :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_sum_idl a exp = eval_exp a exp .=?=. sum_idl where
  sum_idl = eval_exp a (Bin Sum (N 0) exp)

prop_product_idr :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_product_idr a exp = eval_exp a exp .=?=. prod_idr where
  prod_idr = eval_exp a (Bin Product exp (N 1))

prop_product_idl :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_product_idl a exp = eval_exp a exp .=?=. prod_idl where
  prod_idl = eval_exp a (Bin Product (N 1) exp)

prop_e_id :: (Floating a, Real a) => a -> Bool
prop_e_id a = eval_exp a (Un E (N 1)) == expd 1

prop_negate_id :: (Floating a, Real a) => a -> Bool
prop_negate_id a = eval_exp a (Un Negate (N 0)) == 0
\end{code}
    \end{propriedade}
    \begin{propriedade}
      Negar duas vezes uma expressão tem o mesmo valor que não fazer nada.
\begin{code}
prop_double_negate :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_double_negate a exp = eval_exp a exp .=?=. eval_exp a (Un Negate (Un Negate exp))
\end{code}
    \end{propriedade}

  \item É possível otimizar o cálculo do valor de uma expressão aritmética tirando proveito
  dos elementos absorventes de cada operação. Implemente os genes da função
\begin{spec}
      optmize_eval :: (Floating a, Eq a) => a -> (ExpAr a) -> a
\end{spec}
  que se encontra na página \pageref{pg:P1} expressa como um hilomorfismo\footnote{Qual é a vantagem de implementar a função |optimize_eval| utilizando um hilomorfismo em vez de utilizar um catamorfismo com um gene "inteligente"?}
  e teste as propriedades:

    \begin{propriedade}
      A função |optimize_eval| respeita a semântica da função |eval|.
\begin{code}
prop_optimize_respects_semantics :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_optimize_respects_semantics a exp = eval_exp a exp .=?=. optmize_eval a exp
\end{code}
    \end{propriedade}


\item Para calcular a derivada de uma expressão, é necessário aplicar transformações
à expressão original que respeitem as regras das derivadas:\footnote{%
	Apesar da adição e multiplicação gozarem da propriedade comutativa,
	há que ter em atenção a ordem das operações por causa dos testes.}

\begin{itemize}
  \item Regra da soma:
\begin{eqnarray*}
	\frac{d}{dx}(f(x)+g(x))=\frac{d}{dx}(f(x))+\frac{d}{dx}(g(x))
\end{eqnarray*}
  \item Regra do produto:
\begin{eqnarray*}
	\frac{d}{dx}(f(x)g(x))=f(x)\cdot \frac{d}{dx}(g(x))+\frac{d}{dx}(f(x))\cdot g(x)
\end{eqnarray*}
\end{itemize}

  Defina o gene do catamorfismo que ocorre na função
    \begin{quote}
      |sd :: Floating a => ExpAr a -> ExpAr a|
    \end{quote}
  que, dada uma expressão aritmética, calcula a sua derivada.
  Testes a fazer, de seguida:
    \begin{propriedade}
       A função |sd| respeita as regras de derivação.
\begin{code}
prop_const_rule :: (Real a, Floating a) => a -> Bool
prop_const_rule a = sd (N a) == N 0

prop_var_rule :: Bool
prop_var_rule = sd X == N 1

prop_sum_rule :: (Real a, Floating a) => ExpAr a -> ExpAr a -> Bool
prop_sum_rule exp1 exp2 = sd (Bin Sum exp1 exp2) == sum_rule where
  sum_rule = Bin Sum (sd exp1) (sd exp2)

prop_product_rule :: (Real a, Floating a) => ExpAr a -> ExpAr a -> Bool
prop_product_rule exp1 exp2 = sd (Bin Product exp1 exp2) == prod_rule where
  prod_rule =Bin Sum (Bin Product exp1 (sd exp2)) (Bin Product (sd exp1) exp2)

prop_e_rule :: (Real a, Floating a) => ExpAr a -> Bool
prop_e_rule exp = sd (Un E exp) == Bin Product (Un E exp) (sd exp)

prop_negate_rule :: (Real a, Floating a) => ExpAr a -> Bool
prop_negate_rule exp = sd (Un Negate exp) == Un Negate (sd exp)
\end{code}
    \end{propriedade}

\item Como foi visto, \emph{Symbolic differentiation} não é a técnica
mais eficaz para o cálculo do valor da derivada de uma expressão.
\emph{Automatic differentiation} resolve este problema cálculando o valor
da derivada em vez de manipular a expressão original.

  Defina o gene do catamorfismo que ocorre na função
    \begin{spec}
    ad :: Floating a => a -> ExpAr a -> a
    \end{spec}
  que, dada uma expressão aritmética e um ponto,
  calcula o valor da sua derivada nesse ponto,
  sem transformar manipular a expressão original.
  Testes a fazer, de seguida:

    \begin{propriedade}
       Calcular o valor da derivada num ponto |r| via |ad| é equivalente a calcular a derivada da expressão e avalia-la no ponto |r|.
\begin{code}
prop_congruent :: (Floating a, Real a) => a -> ExpAr a -> Bool
prop_congruent a exp = ad a exp .=?=. eval_exp a (sd exp)
\end{code}
    \end{propriedade}
\end{enumerate}

\Problema

Nesta disciplina estudou-se como fazer \pd{programação dinâmica} por cálculo,
recorrendo à lei de recursividade mútua.\footnote{Lei (\ref{eq:fokkinga})
em \cite{Ol18}, página \pageref{eq:fokkinga}.}

Para o caso de funções sobre os números naturais (|Nat0|, com functor |fF
X = 1 + X|) é fácil derivar-se da lei que foi estudada uma
	\emph{regra de algibeira}
	\label{pg:regra}
que se pode ensinar a programadores que não tenham estudado
\cp{Cálculo de Programas}. Apresenta-se de seguida essa regra, tomando como exemplo o
cálculo do ciclo-\textsf{for} que implementa a função de Fibonacci, recordar
o sistema
\begin{spec}
fib 0 = 1
fib(n+1) = f n

f 0 = 1
f (n+1) = fib n + f n
\end{spec}
Obter-se-á de imediato
\begin{code}
fib' = p1 . for loop init where
   loop(fib,f)=(f,fib+f)
   init = (1,1)
\end{code}
usando as regras seguintes:
\begin{itemize}
\item	O corpo do ciclo |loop| terá tantos argumentos quanto o número de funções mutuamente recursivas.
\item	Para as variáveis escolhem-se os próprios nomes das funções, pela ordem
que se achar conveniente.\footnote{Podem obviamente usar-se outros símbolos, mas numa primeira leitura
dá jeito usarem-se tais nomes.}
\item	Para os resultados vão-se buscar as expressões respectivas, retirando a variável |n|.
\item	Em |init| coleccionam-se os resultados dos casos de base das funções, pela mesma ordem.
\end{itemize}
Mais um exemplo, envolvendo polinómios do segundo grau $ax^2 + b x + c$ em |Nat0|.
Seguindo o método estudado nas aulas\footnote{Secção 3.17 de \cite{Ol18} e tópico
\href{https://www4.di.uminho.pt/~jno/media/cp/}{Recursividade mútua} nos vídeos das aulas teóricas.},
de $f\ x = a x^2 + b x + c$ derivam-se duas funções mutuamente recursivas:
\begin{spec}
f 0 = c
f (n+1) = f n + k n

k 0 = a + b
k(n+1) = k n + 2 a
\end{spec}
Seguindo a regra acima, calcula-se de imediato a seguinte implementação, em Haskell:
\begin{code}
f' a b c = p1 . for loop init where
  loop(f,k) = (f+k,k+2*a)
  init = (c,a+b) 
\end{code}
O que se pede então, nesta pergunta?
Dada a fórmula que dá o |n|-ésimo \catalan{número de Catalan},
\begin{eqnarray}
	C_n = \frac{(2n)!}{(n+1)! (n!) }
	\label{eq:cat}
\end{eqnarray}
derivar uma implementação de $C_n$ que não calcule factoriais nenhuns.
Isto é, derivar um ciclo-\textsf{for}
\begin{spec}
cat = cdots . for loop init where cdots
\end{spec}
que implemente esta função.

\begin{propriedade}
A função proposta coincidem com a definição dada:
\begin{code}
prop_cat = (>=0) .==>. (catdef  .==. cat)
\end{code}
\end{propriedade}
%
\textbf{Sugestão}: Começar por estudar muito bem o processo de cálculo dado
no anexo \ref{sec:recmul} para o problema (semelhante) da função exponencial.


\Problema 

As \bezier{curvas de Bézier}, designação dada em honra ao engenheiro
\href{https://en.wikipedia.org/wiki/Pierre_B%C3%A9zier}{Pierre Bézier},
são curvas ubíquas na área de computação gráfica, animação e modelação.
Uma curva de Bézier é uma curva paramétrica, definida por um conjunto
$\{P_0,...,P_N\}$ de pontos de controlo, onde $N$ é a ordem da curva.

\begin{figure}[h!]
  \centering
  \includegraphics[width=0.8\textwidth]{cp2021t_media/Bezier_curves.png}
  \caption{Exemplos de curvas de Bézier retirados da \bezier{ Wikipedia}.}
\end{figure}

O algoritmo de \emph{De Casteljau} é um método recursivo capaz de calcular
curvas de Bézier num ponto. Apesar de ser mais lento do que outras abordagens,
este algoritmo é numericamente mais estável, trocando velocidade por correção.

De forma sucinta, o valor de uma curva de Bézier de um só ponto $\{P_0\}$
(ordem $0$) é o próprio ponto $P_0$. O valor de uma curva de Bézier de ordem
$N$ é calculado através da interpolação linear da curva de Bézier dos primeiros
$N-1$ pontos e da curva de Bézier dos últimos $N-1$ pontos.

A interpolação linear entre 2 números, no intervalo $[0, 1]$, é dada pela
seguinte função:

\begin{code}
linear1d :: Rational -> Rational -> OverTime Rational
linear1d a b = formula a b where
  formula :: Rational -> Rational -> Float -> Rational
  formula x y t = ((1.0 :: Rational) - (toRational t)) * x + (toRational t) * y
\end{code}
%
A interpolação linear entre 2 pontos de dimensão $N$ é calculada através
da interpolação linear de cada dimensão.

O tipo de dados |NPoint| representa um ponto com $N$ dimensões.
\begin{code}
type NPoint = [Rational]
\end{code}
Por exemplo, um ponto de 2 dimensões e um ponto de 3 dimensões podem ser
representados, respetivamente, por:
\begin{spec}
p2d = [1.2, 3.4]
p3d = [0.2, 10.3, 2.4]
\end{spec}
%
O tipo de dados |OverTime a| representa um termo do tipo |a| num dado instante
(dado por um |Float|).
\begin{code}
type OverTime a = Float -> a
\end{code}
%
O anexo \ref{sec:codigo} tem definida a função 
    \begin{spec}
    calcLine :: NPoint -> (NPoint -> OverTime NPoint)
    \end{spec}
que calcula a interpolação linear entre 2 pontos, e a função
    \begin{spec}
    deCasteljau :: [NPoint] -> OverTime NPoint
    \end{spec}
que implementa o algoritmo respectivo.

\begin{enumerate}

\item Implemente |calcLine| como um catamorfismo de listas,
testando a sua definição com a propriedade:
    \begin{propriedade} Definição alternativa.
\begin{code}
prop_calcLine_def :: NPoint -> NPoint -> Float -> Bool
prop_calcLine_def p q d = calcLine p q d ==  zipWithM linear1d p q d
\end{code}
    \end{propriedade}

\item Implemente a função |deCasteljau| como um hilomorfismo, testando agora a propriedade:
    \begin{propriedade}
      Curvas de Bézier são simétricas.
\begin{code}
prop_bezier_sym :: [[Rational]] -> Gen Bool
prop_bezier_sym l = all (< delta) . calc_difs . bezs <$> elements ps  where
  calc_difs = (\(x, y) -> zipWith (\w v -> if w >= v then w - v else v - w) x y)
  bezs t    = (deCasteljau l t, deCasteljau (reverse l) (fromRational (1 - (toRational t))))
  delta = 1e-2
\end{code}
    \end{propriedade}

  \item Corra a função |runBezier| e aprecie o seu trabalho\footnote{%
        A representação em Gloss é uma adaptação de um
        \href{https://github.com/hrldcpr/Bezier.hs}{projeto}
        de Harold Cooper.} clicando na janela que é aberta (que contém, a verde, um ponto
        inicila) com o botão esquerdo do rato para adicionar mais pontos.
        A tecla |Delete| apaga o ponto mais recente.

\end{enumerate}

\Problema

Seja dada a fórmula que calcula a média de uma lista não vazia $x$,
\begin{equation}
avg\ x = \frac 1 k\sum_{i=1}^{k} x_i
\end{equation}
onde $k=length\ x$. Isto é, para sabermos a média de uma lista precisamos de dois catamorfismos: o que faz o somatório e o que calcula o comprimento a lista.
Contudo, é facil de ver que
\begin{quote}
	$avg\ [a]=a$
\\
	$avg (a:x) = \frac 1 {k+1}(a+\sum_{i=1}^{k} x_i) = \frac{a+k(avg\ x)}{k+1}$ para $k=length\ x$
\end{quote}
Logo $avg$ está em recursividade mútua com $length$ e o par de funções pode ser expresso por um único catamorfismo, significando que a lista apenas é percorrida uma vez.

\begin{enumerate}

\item	Recorra à lei de recursividade mútua para derivar a função
|avg_aux = cata (either b q)| tal que 
|avg_aux = split avg length| em listas não vazias. 

\item	Generalize o raciocínio anterior para o cálculo da média de todos os elementos de uma \LTree\ recorrendo a uma única travessia da árvore (i.e.\ catamorfismo).

\end{enumerate}
Verifique as suas funções testando a propriedade seguinte:
\begin{propriedade}
A média de uma lista não vazia e de uma \LTree\ com os mesmos elementos coincide,
a menos de um erro de 0.1 milésimas:
\begin{code}
prop_avg = nonempty .==>. diff .<=. const 0.000001 where
   diff l = avg l - (avgLTree . genLTree) l
   genLTree = anaLTree lsplit
   nonempty = (>[])
\end{code}
\end{propriedade}

\Problema	(\textbf{NB}: Esta questão é \textbf{opcional} e funciona como \textbf{valorização} apenas para os alunos que desejarem fazê-la.) 

\vskip 1em \noindent
Existem muitas linguagens funcionais para além do \Haskell, que é a linguagem usada neste trabalho prático. Uma delas é o \Fsharp\ da Microsoft. Na directoria \verb!fsharp! encontram-se os módulos \Cp, \Nat\ e \LTree\ codificados em \Fsharp. O que se pede é a biblioteca \BTree\ escrita na mesma linguagem.

Modo de execução: o código que tiverem produzido nesta pergunta deve ser colocado entre o \verb!\begin{verbatim}! e o \verb!\end{verbatim}! da correspondente parte do anexo \ref{sec:resolucao}. Para além disso, os grupos podem demonstrar o código na oral.

\newpage

\part*{Anexos}

\appendix

\section{Como exprimir cálculos e diagramas em LaTeX/lhs2tex}
Como primeiro exemplo, estudar o texto fonte deste trabalho para obter o
efeito:\footnote{Exemplos tirados de \cite{Ol18}.} 
\begin{eqnarray*}
\start
	|id = split f g|
%
\just\equiv{ universal property }
%
        |lcbr(
		p1 . id = f
	)(
		p2 . id = g
	)|
%
\just\equiv{ identity }
%
        |lcbr(
		p1 = f
	)(
		p2 = g
	)|
\qed
\end{eqnarray*}

Os diagramas podem ser produzidos recorrendo à \emph{package} \LaTeX\ 
\href{https://ctan.org/pkg/xymatrix}{xymatrix}, por exemplo: 
\begin{eqnarray*}
\xymatrix@@C=2cm{
    |Nat0|
           \ar[d]_-{|cataNat g|}
&
    |1 + Nat0|
           \ar[d]^{|id + (cataNat g)|}
           \ar[l]_-{|inNat|}
\\
     |B|
&
     |1 + B|
           \ar[l]^-{|g|}
}
\end{eqnarray*}

\section{Programação dinâmica por recursividade múltipla}\label{sec:recmul}
Neste anexo dão-se os detalhes da resolução do Exercício \ref{ex:exp} dos apontamentos da
disciplina\footnote{Cf.\ \cite{Ol18}, página \pageref{ex:exp}.},
onde se pretende implementar um ciclo que implemente
o cálculo da aproximação até |i=n| da função exponencial $exp\ x = e^x$,
via série de Taylor:
\begin{eqnarray}
	exp\ x 
& = &
	\sum_{i=0}^{\infty} \frac {x^i} {i!}
\end{eqnarray}
Seja $e\ x\ n = \sum_{i=0}^{n} \frac {x^i} {i!}$ a função que dá essa aproximação.
É fácil de ver que |e x 0 = 1| e que $|e x (n+1)| = |e x n| + \frac {x^{n+1}} {(n+1)!}$.
Se definirmos $|h x n| = \frac {x^{n+1}} {(n+1)!}$ teremos |e x| e |h x| em recursividade
mútua. Se repetirmos o processo para |h x n| etc obteremos no total três funções nessa mesma
situação:
\begin{spec}
e x 0 = 1
e x (n+1) = h x n + e x n

h x 0 = x
h x (n+1) = x/(s n) * h x n

s 0 = 2
s (n+1) = 1 + s n
\end{spec}
Segundo a \emph{regra de algibeira} descrita na página \ref{pg:regra} deste enunciado,
ter-se-á, de imediato:
\begin{code}
e' x = prj . for loop init where
     init = (1,x,2)
     loop(e,h,s)=(h+e,x/s*h,1+s)
     prj(e,h,s) = e
\end{code}

\section{Código fornecido}\label{sec:codigo}

\subsection*{Problema 1}

\begin{code}
expd :: Floating a => a -> a
expd = Prelude.exp

type OutExpAr a = Either () (Either a (Either (BinOp, (ExpAr a, ExpAr a)) (UnOp, ExpAr a)))
\end{code}

\subsection*{Problema 2}
Definição da série de Catalan usando factoriais (\ref{eq:cat}):
\begin{code}
catdef n = div (fac((2*n))) ((fac((n+1))*(fac n)))
\end{code}
Oráculo para inspecção dos primeiros 26 números de Catalan\footnote{Fonte:
\catalan{Wikipedia}.}:
\begin{code}
oracle = [
    1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440, 9694845,
    35357670, 129644790, 477638700, 1767263190, 6564120420, 24466267020,
    91482563640, 343059613650, 1289904147324, 4861946401452
    ]
\end{code}

\subsection*{Problema 3}
Algoritmo:
\begin{spec}
deCasteljau :: [NPoint] -> OverTime NPoint
deCasteljau [] = nil
deCasteljau [p] = const p
deCasteljau l = \pt -> (calcLine (p pt) (q pt)) pt where
  p = deCasteljau (init l)
  q = deCasteljau (tail l)
\end{spec}
Função auxiliar:
\begin{spec}
calcLine :: NPoint -> (NPoint -> OverTime NPoint)
calcLine [] = const nil
calcLine(p:x) = curry g p (calcLine x) where
   g :: (Rational, NPoint -> OverTime NPoint) -> (NPoint -> OverTime NPoint)
   g (d,f) l = case l of
       []     -> nil
       (x:xs) -> \z -> concat $ (sequenceA [singl . linear1d d x, f xs]) z
\end{spec}
2D:
\begin{code}
bezier2d :: [NPoint] -> OverTime (Float, Float)
bezier2d [] = const (0, 0)
bezier2d l = \z -> (fromRational >< fromRational) . (\[x, y] -> (x, y)) $ ((deCasteljau l) z)
\end{code}
Modelo:
\begin{code}
data World = World { points :: [NPoint]
                   , time :: Float
                   }
initW :: World
initW = World [] 0

tick :: Float -> World -> World
tick dt world = world { time=(time world) + dt }

actions :: Event -> World -> World
actions (EventKey (MouseButton LeftButton) Down _ p) world =
  world {points=(points world) ++ [(\(x, y) -> map toRational [x, y]) p]}
actions (EventKey (SpecialKey KeyDelete) Down _ _) world =
    world {points = cond (== []) id init (points world)}
actions _ world = world

scaleTime :: World -> Float
scaleTime w = (1 + cos (time w)) / 2

bezier2dAtTime :: World -> (Float, Float)
bezier2dAtTime w = (bezier2dAt w) (scaleTime w)

bezier2dAt :: World -> OverTime (Float, Float)
bezier2dAt w = bezier2d (points w)

thicCirc :: Picture
thicCirc = ThickCircle 4 10

ps :: [Float]
ps = map fromRational ps' where
  ps' :: [Rational]
  ps' = [0, 0.01..1] -- interval
\end{code}
Gloss:
\begin{code}
picture :: World -> Picture
picture world = Pictures
  [ animateBezier (scaleTime world) (points world)
  , Color white . Line . map (bezier2dAt world) $ ps
  , Color blue . Pictures $ [Translate (fromRational x) (fromRational y) thicCirc | [x, y] <- points world]
  , Color green $ Translate cx cy thicCirc
  ] where
  (cx, cy) = bezier2dAtTime world
\end{code}
Animação:
\begin{code}
animateBezier :: Float -> [NPoint] -> Picture
animateBezier _ [] = Blank
animateBezier _ [_] = Blank
animateBezier t l = Pictures
  [ animateBezier t (init l)
  , animateBezier t (tail l)
  , Color red . Line $ [a, b]
  , Color orange $ Translate ax ay thicCirc
  , Color orange $ Translate bx by thicCirc
  ] where
  a@(ax, ay) = bezier2d (init l) t
  b@(bx, by) = bezier2d (tail l) t
\end{code}
Propriedades e \emph{main}:
\begin{code}
runBezier :: IO ()
runBezier = play (InWindow "Bézier" (600, 600) (0,  0))
  black 50 initW picture actions tick

runBezierSym :: IO ()
runBezierSym = quickCheckWith (stdArgs {maxSize = 20, maxSuccess = 200} ) prop_bezier_sym
\end{code}

Compilação e execução dentro do interpretador:\footnote{Pode ser útil em testes
envolvendo \gloss{Gloss}. Nesse caso, o teste em causa deve fazer parte de uma função
|main|.}
\begin{code}
main = runBezier

run = do { system "ghc cp2021t" ; system "./cp2021t" }
\end{code}

\subsection*{QuickCheck}
Código para geração de testes:
\begin{code}
instance Arbitrary UnOp where
  arbitrary = elements [Negate, E]

instance Arbitrary BinOp where
  arbitrary = elements [Sum, Product]

instance (Arbitrary a) => Arbitrary (ExpAr a) where
  arbitrary = do
    binop <- arbitrary
    unop  <- arbitrary
    exp1  <- arbitrary
    exp2  <- arbitrary
    a     <- arbitrary

    frequency . map (id >< pure) $ [(20, X), (15, N a), (35, Bin binop exp1 exp2), (30, Un unop exp1)]


infixr 5 .=?=.
(.=?=.) :: Real a => a -> a -> Bool
(.=?=.) x y = (toRational x) == (toRational y)


\end{code}

\subsection*{Outras funções auxiliares}
%----------------- Outras definições auxiliares -------------------------------------------%
Lógicas:
\begin{code}
infixr 0 .==>.
(.==>.) :: (Testable prop) => (a -> Bool) -> (a -> prop) -> a -> Property
p .==>. f = \a -> p a ==> f a

infixr 0 .<==>.
(.<==>.) :: (a -> Bool) -> (a -> Bool) -> a -> Property
p .<==>. f = \a -> (p a ==> property (f a)) .&&. (f a ==> property (p a))

infixr 4 .==.
(.==.) :: Eq b => (a -> b) -> (a -> b) -> (a -> Bool)
f .==. g = \a -> f a == g a

infixr 4 .<=.
(.<=.) :: Ord b => (a -> b) -> (a -> b) -> (a -> Bool)
f .<=. g = \a -> f a <= g a

infixr 4 .&&&.
(.&&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .&&&. g = \a -> ((f a) && (g a))
\end{code}

%----------------- Soluções dos alunos -----------------------------------------%

\section{Soluções dos alunos}\label{sec:resolucao}
Os alunos devem colocar neste anexo as suas soluções para os exercícios
propostos, de acordo com o "layout" que se fornece. Não podem ser
alterados os nomes ou tipos das funções dadas, mas pode ser adicionado
texto, disgramas e/ou outras funções auxiliares que sejam necessárias.

Valoriza-se a escrita de \emph{pouco} código que corresponda a soluções
simples e elegantes. 

\subsection*{Problema 1} \label{pg:P1}
São dadas:
\begin{code}
cataExpAr g = g . recExpAr (cataExpAr g) . outExpAr
anaExpAr g = inExpAr . recExpAr (anaExpAr g) . g
hyloExpAr h g = cataExpAr h . anaExpAr g

eval_exp :: Floating a => a -> (ExpAr a) -> a
eval_exp a = cataExpAr (g_eval_exp a)

optmize_eval :: (Floating a, Eq a) => a -> (ExpAr a) -> a
optmize_eval a = hyloExpAr (gopt a) clean

sd :: Floating a => ExpAr a -> ExpAr a
sd = p2 . cataExpAr sd_gen

ad :: Floating a => a -> ExpAr a -> a
ad v = p2 . cataExpAr (ad_gen v)
\end{code}
Definir:

A função de out deste tipo deve receber uma ExpAr e fornecer os dados relevantes
identificando-os apropriadamente de acordo com a inExpAr já utilizada. 

Assim sendo, para expressões de variáveis (X), como não temos nenhum parâmetro, devolvemos 
apenas '()' identificado com Left. Todos os outros tipos de expressões serão então identificadas
Right e mais um  (ou dois para as expressões) identificador para os diferenciar entre eles.
\begin{code}
outExpAr X = i1 ()
outExpAr (N a) = i2 (i1 a)
outExpAr (Bin op ex1 ex2) = i2 (i2 (i1 (op, (ex1, ex2))))
outExpAr (Un op ex) = i2 (i2 (i2 (op, ex)))
\end{code}
---
A função recExpAr está responsável pela recursividade de um catamorfismo. Como tal,
faz sentido que ela aplique a função fornecida como argumento apenas às parte recursivas
das expressões, ou seja, à expressões que estejam dentro do tipo de dados criado pelo out.
Os casos em que temos expressões dentro do nosso tipo de dados são os casos das operações,
nos quais a outExpAr devolve  o (devidamente identificado) par de operação expressão/expressões.
Sendo assim, esta função tem de aplicar ao tipo de dados a função fornecida às expressões das possíveis
operações, deixando outros identificadores iguais (id). Usando a função baseExpAr, basta indicarmos
onde aplicar id e onde aplicar a função fornecida.
\begin{code}
recExpAr f = baseExpAr id id id f f id f
\end{code}
---
Num catamorfismo de ExpAr, o gene vai sempre receber o tipo 1 + (B + (C + D)), em que
o 1 corresponde a uma expressão de variável, B a uma constante (N 0, por exemplo), C a uma
operação binária e D a uma operação unária. Sendo assim, o nosso gene de eval
tem de devolver o float fornecido para variáveis, o próprio valor para constantes, e 
finalmente operar segundo as operações que encontrar (que definimos nas funções auxiliares)
do_bin e do_un.

\begin{code}
g_eval_exp a = either (const a) (either id do_ops) where
  do_ops = either do_bin do_un
  do_bin :: Num a => (BinOp, (a, a)) -> a
  do_bin (Sum, pair) = (uncurry (+)) pair
  do_bin (Product, pair) = (uncurry (*)) pair
  do_un :: Floating a => (UnOp, a) -> a
  do_un (Negate, x) = negate x
  do_un (E, x) = exp x
\end{code}
---
Para este hilomorfismo, o nosso anamorfismo vai tratar de limpar possíveis casos
de absorção nas operações das expressões fornecidas, sendo assim responsável pela
parte do algoritmo de otimização. O catamorfismo utilizado é o que definimos na alínea
anterior, sendo responsável por calcular o valor de uma expressão. 

\begin{code}
clean :: (Floating a, Eq a) => ExpAr a -> Either () (Either a (Either (BinOp, (ExpAr a, ExpAr a)) (UnOp, ExpAr a)))
clean = outExpAr . clean_aux where
  clean_aux :: (Floating a, Eq a) => ExpAr a -> ExpAr a
  clean_aux (Bin Product (N 0) _) = N 0
  clean_aux (Bin Product _ (N 0)) = N 0
  clean_aux (Un E (N 0)) = N 1
  clean_aux e = e
---
gopt a = g_eval_exp a

\end{code}

A função sd já implementada executa um catamorfismo que devolve um par e usa a função
p2 para usar apenas o segundo elemento do par. O par contém no elemento da esquerda
a expressão original e no elemento da direita a expressão derivada. Como tal, o nosso
gene terá de devolver esse par. Sendo assim, o nosso gene será um split. À esquerda do split
será trabalhado o tipo de dados para remover a informação relativa à derivada (Ou seja,
serão trabalhadas as expressões para que tenham apenas a informação relativa à expressão
original). Após esta tarefa, o tipo de dados passa pelo inExpAr e é transformado de volta numa
expressão. O lado direito do split fica então responsável por aplicar as leis da derivação
aos casos diferentes de expressões, utilizando onde necessária a informação relativa às expressões
originais ou às derivadas de expressões em operações calculadas recursivamente.
\begin{code}
sd_gen :: (Floating a) =>
    Either () (Either a (Either (BinOp, ((ExpAr a, ExpAr a), (ExpAr a, ExpAr a))) (UnOp, (ExpAr a, ExpAr a)))) -> (ExpAr a, ExpAr a)
sd_gen = split (inExpAr.og) (der) where
  og = id -|- (id -|- ((id >< (split (p1.p1) (p1.p2))) -|- (id >< p1)))
  --der = either (const (N 1)) (either (const (N 0)) der_aux)
  --der_aux = inExpAr . (either (i2.i2.i1.(id >< (split (p2.p1) (p2.p2)))) (i2.i2.i2.(id >< p2)))
der :: (Floating a) => Either () (Either a (Either (BinOp, ((ExpAr a, ExpAr a), (ExpAr a, ExpAr a))) (UnOp, (ExpAr a, ExpAr a)))) -> ExpAr a
der = either (inExpAr.i2.i1.(const 1)) (either (const (N 0)) (either bin_aux un_aux))
bin_aux :: (Floating a) => (BinOp, ((ExpAr a, ExpAr a), (ExpAr a, ExpAr a))) -> ExpAr a
bin_aux (Sum,((o1, d1),(o2, d2))) = (Bin Sum d1 d2)
bin_aux (Product,((o1, d1),(o2, d2))) = (Bin Sum (Bin Product o1 d2) (Bin Product d1 o2))
un_aux :: (Floating a) => (UnOp, (ExpAr a, ExpAr a)) -> ExpAr a
un_aux (E,(o, d)) = (Bin Product (Un E o) d)
un_aux (Negate, (o, d)) = (Un Negate d)
\end{code}


A função ad, tal como a sd, usa um catamorfismo que devolve um par e apenas
usa o segundo elemento do par. Este par contém na esquerda o valor da expressão
original e na direita o valor da expressão derivada. Como tal, o nosso gene vai ser um split
em que à esquerda trabalhamos com os valores calculados recursivamente para a expressão original,
e à direita aplicamos as leis da derivação relevantes para calcular a derivada usando valores
calculados recursivamente nas expressões de operações. As funções auxiliares x_ad trabalham
todas sobre um tipo de expressão, aplicando as leis da derivação relevantes para cada tipo.

\begin{code}
ad_gen :: Floating a => a -> Either () (Either a (Either (BinOp, ((a, a),(a,a))) (UnOp, (a,a)))) -> (a,a)
ad_gen v = either (var_ad v) (either const_ad (either bin_ad un_ad))
var_ad :: Floating a => a -> () -> (a, a)
var_ad v = split (const v) (const 1)
const_ad :: Floating a => a -> (a, a)
const_ad = split id (const 0)
bin_ad :: Floating a => (BinOp, ((a,a),(a,a))) -> (a,a)
bin_ad (Sum, ((o1, d1),(o2, d2))) = ((o1+o2), (d1+d2))
bin_ad (Product, ((o1, d1),(o2, d2))) = ((o1*o2), (o1*d2 + d1*o2))
un_ad :: Floating a => (UnOp, (a,a)) -> (a,a)
un_ad (Negate, (o, d)) = ((-o), (-d))
un_ad (E, (o, d)) = ((exp o), ((exp o) * d))

\end{code}


\subsection*{Problema 2}

Incrementa os denominaodes. O caso de paragem do d é 1 e o caso de paragem desta função dá 2 porque estamos a trabalhar
no padrão n+1, ou seja, quando fazemos 1 (caso paragem do d) + 1 dá 2 que corresponde ao caso de paragem do di.
Aqui estamos a somar 1 porque estamos no padrão n+1 porque é como se tivessemos a somar começando no 1 (sem ser no caso de paragem),
ou seja, 0+1 fica 1 por isso corresponde a 1 + numei.

\begin{code}
denominator_incrementer 0 = 2
denominator_incrementer (n + 1) = 1 + denominator_incrementer n
\end{code}

Denominador

\begin{code}
denominator 0 = 1
denominator (n + 1) = denominator_incrementer n * denominator n
\end{code}

Incrementa os numeradores sucessivos. O caso de paragem do nume é 1 e o caso de paragem desta função dá 3 porque estamos a trabalhar
no padrão n+1, ou seja, quando fazemos 1 (caso paragem do nume) + (1+1) dá 3 que corresponde ao caso de paragem do numei.
Aqui estamos a somar 2 porque estamos no padrão n+1 porque é como se tivessemos a somar começando no 1 (sem ser no caso de paragem),
ou seja, 1+1 fica 2 por isso corresponde a 2 + numei.

\begin{code}
numerator_incrementer 0 = 3
numerator_incrementer (n + 1) = 2 + numerator_incrementer n
\end{code}

Numerador

\begin{code}
numerator 0 = 1
numerator (n + 1) = numerator_incrementer n * numerator n
\end{code} 

Fração que corresponde ao quociente entre o numerador e o denominador

\begin{code}
quotient 0 = 1
quotient n = numerator n / denominator n
\end{code}  

Na primeira componente temos o quociente entre o numero e o denominador. 
Na segunda compoente temos o produto do numerador pelo seu numerador sucessivo + 1 pelo numerador sucessivo para obter o numerador pretendido.
Na terceira compoente temos o produto do denominador pelo seu denominadorsucessivo + 1 pelo denominador sucessivo para obter o 
numerador pretendido.
A quarta componente corresponde à função numei para incrementar os numeradores sucessivos.
A quinta componente corresponde à função a para incrementar os denominadores sucessivos.

\begin{code}
loop (quotient , numerator , denominator , numerator_incrementer , denominator_incrementer) = (div numerador denominator , numerator * (numerator_incrementer + 1) * numerator_incrementer , denominator * (denominator_incrementer + 1) * denominator_incrementer , numerator_incrementer + 2 , denominator_incrementer + 1)  
\end{code} 

Valores dos casos de paragem de cada uma das funções

\begin{code}
inic = (1 , 1 , 1 , 3 , 2)
\end{code} 

Função principal é a f

\begin{code}
prj (f , g , h , i , j) = f 
\end{code}  

por forma a que
\begin{code}
cat = prj . (for loop inic)
\end{code}
seja a função pretendida.
\textbf{NB}: usar divisão inteira.

\subsection*{Problema 3}
A resoluão da primeira alínea passar por adaptarmos o algoritmo fornecido a um catamorfismo.
Usando a função auxiliar g usada na função fornecida, basta criarmos um catamorfismo de listas
que aplica essa função auxiliar a listas não vazias e devolve a lista vazia para listas também vazias
(Ou seja, pontos de dimensão 0).

\begin{code}
g :: (Rational, NPoint -> OverTime NPoint) -> (NPoint -> OverTime NPoint)
g (d,f) l = case l of
  []      -> nil
  (x:xs)  -> \z -> concat $ (sequenceA [singl . linear1d d x, f xs]) z

calcLine :: NPoint -> (NPoint -> OverTime NPoint)
calcLine = cataList gene where
    gene = either (const (const nil)) g
    g :: (Rational, NPoint -> OverTime NPoint) -> (NPoint -> OverTime NPoint)
    g (d,f) l = case l of
      []      -> nil
      (x:xs)  -> \z -> concat $ (sequenceA [singl . linear1d d x, f xs]) z

\end{code}
Para a segunda alínea, decidimos utilizar um hilomorfismo de LTree. O anamorfismo
será responsável por, para listas de NPoints não vazias (sendo o caso da lista)
vazia tratada por pattern matchin na função deCasteljau), gerar uma LTree em que
o ramo da esquerda corresponde ao init da lista e o ramo da direita corresponde à
tail da lista.  

Este é o passo de "divide" do nosso hilomorfismo, em que dividimos a
lista de pontos até só termos um ponto em cada nodo. 

O nosso catamorfismo é assim a fase de conquer, em que resolvemos os problemas
que foram reduzidos no divide. Usamos a função auxiliar utilizada na função deCasteljau
fornecida para transformar os pontos nos pares de leafs das ltree, transformando gradualmente
a LTree num OverTime NPoint através do cálculo de baixo para cima das curvas.

\begin{code}
outNList :: [a] -> Either a (a, [a])
outNList [a]   = i1 a
outNList (a:x) = i2 (a,x)

deCasteljau :: [NPoint] -> OverTime NPoint
deCasteljau [] = nil
deCasteljau l = (hyloAlgForm alg coalg) l where
    coalg = ((id) -|- (split (init.cons) p2)) . outNList
    alg = either (const) (g_aux)
    g_aux :: (OverTime NPoint, OverTime NPoint) -> OverTime NPoint
    g_aux (ot1, ot2)= \pt -> (calcLine (ot1 pt) (ot2 pt)) pt

hyloAlgForm f g = (cataLTree f) . (anaLTree g)
\end{code}


\subsection*{Problema 4}

Solução para listas não vazias:

Para a resolução da primeira alínea, resolvemos o problema utilizando a lei da recursividade mútua (\textit{Fokkinga}). Assim sendo, desenvolvemos da seguinte forma:

|avg_aux = <avg, len>|

Logo, introduzindo o catamorfismo...
\begin{eqnarray*}
\start
  |<avg, len> = cata (split h k)|
%
\just\equiv{ Lei Fokkinga, F f = id + id x f para listas }
%
          |lcbr(
      avg . in = h . (id + id x <avg, len>)
    )(
      len . in = k . (id + id x <avg, len>)
    )|
%
\end{eqnarray*}
Desenvolvendo o sistema, separando o h e o k em h1 e h2, k1 e k2, e introduzindo variáveis, ficamos com as seguintes equações:
\begin{eqnarray*}
\start
          |lcbr(
      lcbr(
  avg [] = 0
      )(
  avg (a:as) = (a + (avg as * len as)) / (len as + 1)
      ))(
      lcbr(
  len [] = 0
      )(
  len (a:as) = 1 + len as
      ))|
\end{eqnarray*}
Finalmente removemos as variáveis novamente e definimos a nossa implementação efetiva em haskell.
\begin{eqnarray*}
\start
          |lcbr(
        lcbr(
  avg . nil  = const 0
        )(
  avg . cons = mydiv . <myadd.<p1 (mymul.p2)> k2>
        ))(
        lcbr(
  len . nil = const 0
        )(
  len . cons = succ . p2 . p2
        ))|
\end{eqnarray*}
Em que definimos, pela seguinte ordem, h1, h2, k1, e k2. Usando a lei da troca, chegamos ao formato pedido no enunciado.
\begin{eqnarray*}
\start
|<avg, len> = cata (<[h1, h2], [h1, h2]>)|
%
\just\equiv{ Lei da troca}
%
|<avg, len> = cata [<h1, k1>, <h2, k2>]|
%
\just\equiv{ Definições de h e k }
%
|<avg, len> = cata [<const 0, const 0>, <mydiv.<myadd.<p1, (mymul.p2)>, k2>, succ.p2.p2>]|
\end{eqnarray*}
E finalmente chegamos à solução da alínea. São definidas e utilizadas as funções mydiv, etc devido a problemas com o interpretador no uso das funções já definidas nos módulos fornecidos.
\begin{code}
avg = p1.avg_aux
\end{code}

\begin{code}
mydiv = (uncurry (/))
myadd = (uncurry (+))
mymul = (uncurry (*)) 

avg_aux = cataList (either (split (const 0) (const 0)) (split h2 k2)) where
    k2 = succ.p2.p2
    h2 = mydiv . (split (myadd.(split p1 (mymul.p2))) k2)
\end{code}
Solução para árvores de tipo \LTree:

Para este caso olhamos para o diagrama do catamorfismo em questão. Esta alínea passa por adaptar o nosso gene ao catamorfismo da LTree.


\xymatrix@@C=2cm{
    |LTree(Nat0)|
           \ar[d]_-{|cataNat gene|}
&
    |Nat0 + LTree(Nat0) >< LTree(Nat0)|
           \ar[d]^{|id + (cata gene) >< (cata gene)|}
           \ar[l]_-{|in|}
\\
     |(A, A)|
&
     |Nat0 + (A, A) >< (A, A)|
           \ar[l]^-{|gene = [<h1, k1>, <h2, k2>]|}
}

Definimos então h1 e k1 como a média e comprimento de uma Leaf e h2 e k2 como média e comprimento de um Fork, que são definições facilmente implementadas em haskell.
\begin{code}
avgLTree = p1.cataLTree gene where
    k2 = myadd . (split (p2.p1) (p2.p2))
    h2 = mydiv . (split (myadd . (split (mymul.p1) (mymul.p2))) k2)
    gene = either (split id (const 1)) (split h2 k2)
\end{code}


\subsection*{Problema 5}
Inserir em baixo o código \Fsharp\ desenvolvido, entre \verb!\begin{verbatim}! e \verb!\end{verbatim}!:


\begin{verbatim}
module cp2021t

open Cp


// (1) Datatype definition -----------------------------------------------------
//data BTree a = Empty | Node(a, (BTree a, BTree a)) deriving Show
type BTree<'a> = Empty | Node of 'a * (BTree<'a> * BTree<'a>)

let inBTree a = either (konst Empty) Node x 

let outBTree a = 
    match a with 
        | Empty -> Left ()
        | (Node(a , (t1 , t2))) -> Right (a , (t1 , t2))

// (2) Ana + cata + hylo -------------------------------------------------------
let recBTree g = baseBTree id g

let cataBTree g a = (g << (recBTree (cataBTree g)) << outBTree) a

let anaBTree g a = (inBTree << (recBTree (anaBTree g) ) << g) a

let hyloBTree h g = cataBTree h << anaBTree g

let baseBTree f g = id -|- (f >< (g >< g))

// (3) Map ---------------------------------------------------------------------
(*
    instance Functor BTree
            where fmap f = cataBTree ( inBTree . baseBTree f id )
*) 

// (4) Examples ----------------------------------------------------------------
// (4.1) Inversion (mirror) ----------------------------------------------------
let invBTree a = cataBTree (inBTree << (id -|- (id >< swap))) a

// (4.2) Counting --------------------------------------------------------------

let countBTree a = cataBTree (either (konst 0) (succ << (uncurry (+)) << p2)) a 

// (4.3) Serialization ---------------------------------------------------------
let inordt = cataBTree inord    

let inord a = either nil join

let join (x,(l,r)) = l @ [x] @ r

let preordt = cataBTree preord

let preord = (either nil preord_g)

let preord_g (x,(l,r)) = x :: l @ r

let postordt = cataBTree (either nil postordt_g)

let postordt_g (x,(l,r)) = l @ r @ [x]

// (4.4) Quicksort -------------------------------------------------------------
let qSort a = hyloBTree inord qsep

//let qsep [] = Left ()
//let qsep list = Right (list.Head part (fun a -> a < list.Head) list.Tail) 

let qsep list = 
    match list with
        | list.isEmpty -> Left ()
        | _            -> Right (list.Head part ( fun a -> a < list.Head)  list.Tail)

let part p [] = ([],[])
let rec part p list =
    match list.Length with
        | p list.Head -> let (s,l) = part p list.Tail in (list.Head @ s, l) 
        | otherwise   -> let (s,l) = part p list.Tail in (s, list.Head @ l)

// (4.5) Traces ----------------------------------------------------------------

let traces = cataBTree (Either (konst [[]]) tunion)

let tunion (a, (l,r)) = union (map (fun x -> a @ x) l) (map (fun x -> a @ x) r)

// (4.6) Towers of Hanoi -------------------------------------------------------

let hanoi = hyloBTree present strategy

// where

let present = inord

let strategy d n = 
    match n with
        | 0     -> Left ()
        | _     -> Right ((n,d), ((not d,n), (not d,n)))

// (5) Depth and balancing (using mutual recursion) --------------------------

let balBTree = p1 . baldepth

let depthBTree = p2 . baldepth

let baldepth = cataBTree depbal

let depbal = Either (konst (True, 1)) ( (fun (a,((b1,b2),(d1,d2))) -> (b1 && b2 && abs(d1-d2)<=1,1+max d1 d2)) . ( id  ><  (fun ((b1,d1),(b2,d2)) -> ((b1,b2),(d1,d2))))

// (6) Going polytipic -------------------------------------------------------

// (7) Zipper ----------------------------------------------------------------
type Deriv<'a> = Dr Bool 'a * (Btree<'a>)

type Zipper a = [ Deriv a ]

let rec plug list t =
    match list with
        | list.isEmpty  -> t
        | _             -> match list.Head with 
                            | Dr (False a l) -> Node (a, (plug list.Tail t, l))
                            | Dr (True a r)  -> Node (a, (r, plug list.Tail t))
\end{verbatim}

%----------------- Fim do anexo com soluções dos alunos ------------------------%

%----------------- Índice remissivo (exige makeindex) -------------------------%

\printindex

%----------------- Bibliografia (exige bibtex) --------------------------------%

\bibliographystyle{plain}
\bibliography{cp2021t}

%----------------- Fim do documento -------------------------------------------%
\end{document}
