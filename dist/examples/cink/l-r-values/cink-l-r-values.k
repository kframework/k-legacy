require "../basic/cink-basic.k"

module CINK-L-R-VALUES-SYNTAX

  imports CINK-BASIC-SYNTAX

  syntax Exp ::= "++" Exp [incr-pref]
               | Exp "++" [incr-post] 

  
  syntax priorities  incr-pref  > assign read write greatthan lessequal plus 
                                  minus multiply divide
  syntax priorities  incr-post  > assign read write greatthan lessequal plus 
                                  minus multiply divide

  syntax Exp ::= Exp ":+:" Exp [myplus]

  syntax priorities incr-pref > myplus

  syntax priorities myplus > assign
 
endmodule

module CINK-L-R-VALUES-SEMANTICS

  imports CINK-L-R-VALUES-SYNTAX

  /*@
  We first deal only with lvalues and prvalues. In order to keep the definition
  as simple as possible, we let the the prvalue as the default category for 
  expressions and we make explicitly only the category of lvalue expressions:
  */
  
  syntax K ::= "lvalue" "(" K ")"

  /*@
  As its definition says, the main operator emphasiszing the two categories is
  the assignment. We use a context declaration to say that the left-hand side 
  of the assignemnt is a lvalue:
  */

  context (HOLE => lvalue(HOLE)) = _:Exp

  /*@
  The above declaration is equivalent with the following to heating/cooling
  rules:
  \begin{align}
  & E_1 \texttt{~=~} E_2 \Rightarrow lvalue(E_1) \kra HOLE \texttt{~=~} E_2\\
  &  lvalue(L_1) \kra HOLE \texttt{~=~} E_2 \Rightarrow L_1 \texttt{~=~} E_2
  \end{align}
  This assume that the locations must be defined as values:
  */

  syntax Val ::= Loc 

  /*@
  An identifier seen as a lvalue is evaluated to its location: 
  */
  rule <k> lvalue(X:Id => L) ...</k>
       <env>... X |-> L:Loc ...</env>
       [lookup]

  /*@
  An assignemnt, after the reduction of the arguments, has in the lef-hand
  side a location ($=$ the lvalue designated by the lhs.)
  */
  rule <k> L:Loc = V:Val  => V ...</k>
       <store>... L |-> (_ => V) ...</store>
       [update]

  /*@
  The right-hand side of reading operator is evaluated as a lvalue:
  */
  context cin >> (HOLE => lvalue(HOLE))

  /*@
  The semantics of the reading operator is overriden:
  */
  rule <k> cin >> L:Loc => cin  ...</k>
       <store>... L |-> (_ => V)  ...</store>
       <in> (ListItem(V:Int) => .)  ...</in>
       [read-stream]

  /*@ \subsubsection{The prefix increment operator}
    Here is the description of this operator from the manual of 
    \Cpp 2011 (5.3.2):\\
    "The operand of prefix ++ is modified by adding 1, or set to true 
    if it is bool (this use is deprecated). The operand shall be a 
    modifiable lvalue. The type of the operand shall be an arithmetic 
    type or a pointer to a completely-defined object type. 
    The result is the updated operand; it is an lvalue, and it is a 
    bit-field if the operand is a bit-field."\\
    This definition raises some problems.
    We suppose that we want to evaluate the
    expression \texttt{y + (++x)}; since the addition operator is strict in 
    both arguments, it will be reduced something like $\it I + lL$ and 
    in that case $L$ should be evaluated to the value stored into
    the location $L$. On the other hands, the evaluation of the expression
    \texttt{++ (++x)} uses the l-value returned by the operator.
    This is one reason for that we make a clear distinction
    between r-value and l-value expressions and use the contexts declarations
    in order to specify how to valuate subexpressions.
  */

  context ++ (HOLE => lvalue(HOLE))

//  context lvalue(++ (HOLE => lvalue(HOLE)))

//  rule lvalue(++ E:Exp) => ++ E ~> lvalue(HOLE) [heat]

  rule <k> ++ L:Loc => L ...</k>
       <store>... L |-> (V:Int => V +Int 1) ...</store>


  rule E1:Exp :+: E2:Exp => E1 ~> HOLE :+: E2 [heat]

  rule E1:Exp :+: E2:Exp => E2 ~> E1 :+: HOLE [heat]

  rule V:Int ~> HOLE :+: E:Exp => V :+: E  [cool]

  rule V:Int ~> E:Exp :+: HOLE => E :+: V  [cool]

  rule I1:Int :+: I2:Int => I1 +Int I2

  rule <k> L:Loc => V ...</k>
       <store>... L |-> V ...</store>

  /*@
  \subsubsection{The postfix increment operator}
   Here is the description of this operator from the manual of 
  \Cpp 2011 (5.2.6):
  "The value of a postfix ++ expression is the value of its operand.
  [ Note: the value obtained is a copy of
  the original value —end note ] The operand shall be a modifiable lvalue.
  The type of the operand shall be an arithmetic type or a pointer to a
  complete object type. The value of the operand object is modified by
  adding 1 to it, unless the object is of type bool, in which case it is set
  to true. [ Note: this use is deprecated, see Annex D. —end note ]
  The value computation of the ++ expression is sequenced before the
  modification of the operand object. With respect to an 
  indeterminately-sequenced function call, the operation of postfix
  ++ is a single evaluation.
  [ Note: Therefore, a function call shall not intervene between the
  lvalue-to-rvalue conversion and the side effect associated with any single
  postfix ++ operator. —end note ] The result is a prvalue."
  From the Appendix D: \\
  "D.1 The use of an operand of type bool with the ++ operator is deprecated 
  (see 5.3.2 and 5.2.6)."
  */

  context (HOLE => lvalue(HOLE)) ++
  rule <k> L:Loc ++ => V ...</k>
       <store>... L |-> (V:Int => V +Int 1) ...</store>

endmodule
