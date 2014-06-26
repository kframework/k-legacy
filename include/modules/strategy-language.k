// Copyright (c) 2014-2014 K Team. All Rights Reserved.

require "strategy-core.k"

module STRATEGY-LANGUAGE 
  imports STRATEGY-CORE
  
  syntax Strategy ::= Bool
  
  // hooking into the stuck mechanism
  rule 
    <s> X => false ... </s> 
    when #stuck andBool notBool(isBool(X,,.KList))

  // cooling rules
  rule 
    <s> true ~> S:Strategy => S[ true | SHOLE ] ...</s> 
    <k> X ~> E => #visit(E, 'replaceHoleWithCurrent, X, 'ieMHole) ...</k> 
    //   E[X | MHOLE(_)] ...</k>
  rule 
    <s> false ~> S:Strategy => S[ false | SHOLE ] ...</s> 
    <k> X ~> E => #visit(E, 'replaceHoleWithPrevious, 'ieMHole) ...</k>

  // nicer way of saying the rules above
  // rule <s> true ~> S => S[true/SHOLE(_)] ...</s> <k> X ~> E => E[X/MHOLE(_)] ...</k>
  // rule <s> false ~> S => S[false/SHOLE(_)] ...</s> <k> _ ~> E => E[X/MHOLE(X)] ...</k>    
  
  syntax Strategy ::= "if" Strategy "then" Strategy "else" Strategy
  
  rule <s> if true then B else C => B ...</s>
  rule <s> if false then B else C => C ...</s>
  
  // heating, could be generated automatically
  rule 
    <s> if A:Strategy then B else C => A ~> if SHOLE then B else C ... </s>
    <k> E => E ~> MHOLE(E) ... </k>
    when (A =/=K SHOLE) andBool notBool(isBool(A,,.KList))
    
    
  syntax Strategy ::= Strategy "<+" Strategy
  rule <s> A <+ B => if A then true else B ...</s>

  syntax Strategy ::= Strategy ";" Strategy
  rule <s> A ; B => if A then B else false ...</s>

  syntax Strategy ::= repeat(Strategy) 
  rule <s> repeat(A) => if A then repeat(A) else true ...</s>

  syntax Strategy ::= child(Strategy)
  rule 
    <s> child(A:Strategy) => A ~> child(SHOLE) ... </s>
    <k> L:KLabel(B:KItem) => B ~> L(MHOLE(B)) ... </k>
    when notBool(isBool(A,,.KList))
    
  rule <s> child(true) => true ... </s>
  rule <s> child(false) => false ... </s>  

endmodule
