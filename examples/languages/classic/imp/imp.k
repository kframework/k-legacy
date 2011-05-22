
kmod IMP-SYNTAX is
/*@ \section{Syntax}
This module defines the syntax of IMP.  There are three syntactic
categories: arithmetic expressions, Boolean expressions, and statements.
We considered a minimal number of constructs in IMP, each showing
something different. */

/*@ We first include the builtin integers and identifiers, which are particular
arithmetic expressions. */

  including PL-INT + PL-ID + K-LATEX

/*@ \subsection{Arithmetic Expressions}
In addition to integers and identifiers, the IMP expressions also include
an addition and a division construct.  We did not include other constructs,
e.g. multiplication, because they are essentially almost identical to addition
or to division; note that division is different from addition, because of
division-by-zero. */

  syntax AExp ::= Id | Int
                | AExp + AExp                  [prec 33 gather (E e) strict]
                | AExp / AExp                  [prec 31 gather (E e) strict]

/*@ \subsection{Boolean Expressions}
We only consider three basic Boolean constructs (one relational operator,
negation, and a short-circuited conjunction), in addition to the builtin
Booleans.  The module of builtin Booleans (PL-BOOL) does not need to be
explicitly included, because is is implicitly included via the builtin
integers.  Note that the Boolean expression construct \texttt{and} is
"strict(1)" instead of just "strict"; that is because we want to give
it a short-circuited semantics. */

  syntax BExp ::= Bool
                | AExp <= AExp                 [prec 37 strict]
                | not BExp                     [prec 53 strict]
                | BExp and BExp                [prec 55 strict(1)]

/*@ \subsection{Statements}
IMP has five statement constructs: the empty statement, assignment,
sequential composition, conditional and while.  Note that ";" is a
statement terminator instead of a statement separator, the latter
being a white space (or more).  Note that the conditional is strict
only in its condition, and that \texttt{while} is not strict at all. */

  syntax Stmt ::= skip ;
                | Id = AExp ;                  [prec 40 strict(2)]
                | Stmt Stmt                    [prec 60 gather (e E)]
                | if BExp then Stmt else Stmt  [prec 59 strict(1)]
                | while BExp do Stmt           [prec 59]
endkm


kmod IMP-PROGRAMS is including IMP-SYNTAX
  syntax Id   ::= a | b | c | d | e | f | g | h | i | j | k | l | m
                | n | o | p | q | r | s | t | u | v | w | x | y | z
  syntax Stmt ::= sumPgm
                | collatzPgm | collatzStmt
                | multiplicationStmt | primalityStmt | countPrimesPgm

macro sumPgm = (
     n=10;
     s=0;
     while not(n<=0) do (
       s=s+n;
       n=n+-1;
     )
)

macro collatzStmt = (
     while not (n<=1) do (
       s=s+1;
       q=n/2;
       r=q+q+1;
       if r<=n
         then n=n+n+n+1; else n=q;
     )
)

macro collatzPgm = (
   m=5;
   s=0;
   while not(m<=2) do (
     n=m;
     m=m+-1;
     collatzStmt
   )
) 

--- fast multiplication (base 2) algorithm
macro multiplicationStmt = ( 
  z=0;
  while not(x<=0) do (
    q=x/2;
    r=q+q+1;
    if r<=x then z=z+y;
    else skip;
    x=q;
    y=y+y;
  )
) 

macro primalityStmt = (
  i=2;
  q=n/i;
  t=1;
  while (i<=q and 1<=t) do (
    x=i;
    y=q;
    multiplicationStmt 
    if n<=z then t=0; else (
      i=i+1;
      q=n/i;
    )
  )
) 

macro countPrimesPgm = (
   m=10;
   s=0; n=2;
   while n<=m do (
     primalityStmt
     if 1<=t then s=s+1; else skip;
     n=n+1;
   )
)

endkm


kmod IMP-SEMANTICS is including IMP-SYNTAX + K
/*@ \section{Semantics}
This module defines the entire \K semantics of IMP.  Recall that a \K
semantics consists of three steps: defining the configuration, defining
the \K results, and defining the semantic rules.  These can be defined
in any order, though we typically first define the configuration and the
results (in any order) and then the rules. */

/*@ \subsection{Configuration}
The configuration of IMP is very simple: it only contains a computation
cell labeled \textsf{k} and a state cell labeled \textsf{state}. */

  configuration <T color="yellow">
                  <k color="green"> .K </k>
                  <state color="red"> .Map </state>
                </T>

/*@ \subsection{Results}
IMP only has two types of results: integers and Booleans. */

  syntax KResult ::= Int | Bool

/*@ \subsection{Arithmetic Expressions}
The \K semantics of each arithmetic construct is defined below.
We need to do nothing for integers. */

/*@ \subsubsection{Variable Lookup}
A program variable $X$ is looked up in state $\Large\sigma$ using the
builtin map lookup operation 
*/
  rule <k> X:Id => Sigma(X) <_/k> <state> Sigma:Map </state>
  rule I1:Int + I2:Int => I1 +Int I2
  rule I1 / I2 => I1 /Int I2  if I2 =/=Bool 0 


  rule I1 <= I2 => I1 <=Int I2
  rule not(T:Bool) => notBool T  
  rule true and B:BExp => B  
  rule false and _ => false  
  rule skip; => .
  rule <k> X=I:Int; => . <_/k> <state> Sigma => Sigma[I/X] </state> 
  rule S1:Stmt S2:Stmt => S1~>S2
  rule if true then S:Stmt else _ => S
  rule if false then _ else S => S
  rule <k> while B do S => if B then (S while B do S) else skip; <_/k>
endkm

kmod IMP is including IMP-PROGRAMS + IMP-SEMANTICS 
  syntax Bag ::= [[ KLabel ]] [latex "\llbracket\,{#1}\,\rrbracket"]
  macro [[KL:KLabel]] = <T_> <k> KL(.List{K}) </k> <_/T>
endkm
