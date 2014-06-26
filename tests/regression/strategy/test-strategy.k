// Copyright (c) 2014-2014 K Team. All Rights Reserved.

require "modules/k-functional-visitor.k"
require "modules/strategy-language.k"

module LAMBDA-SYNTAX
  syntax Val ::= Id
            | "lambda" Id "." Exp  [binder, latex(\lambda{#1}.{#2})]
  
  syntax Exp ::= Val
            | Exp Exp              [left]
            | "(" Exp ")"          [bracket]
            
  syntax KResult ::= Val
  
  syntax priorities 'lambda_._ > '__
endmodule

module LAMBDA-SEMANTICS
  imports LAMBDA-SYNTAX
  imports SUBSTITUTION
  
  // hack in 
  syntax Id ::= "beta" | "ab" | "bc" | "aa"
  
  // for testing
  syntax Exp ::= "x" | "y" | "z" | func(Exp)

  rule <s> aa => true ... </s> <k> x => x ...</k>  
  rule <s> ab => true ... </s> <k> x => y ...</k>
  rule <s> bc => true ... </s> <k> y => z ...</k>
  
  rule <s> beta => true ... </s> <k> lambda X . B E => B [ E | X ] ...</k>
  
endmodule

module LAMBDA-STRATEGY
 imports LAMBDA-SYNTAX
 imports STRATEGY-LANGUAGE 
endmodule

module TEST-STRATEGY
 imports LAMBDA-SYNTAX
 imports LAMBDA-SEMANTICS
 imports LAMBDA-STRATEGY

 configuration <k> $PGM:Exp </k> <s> $STG:Strategy </s> when #stuck
 
 // (lambda x . x)(lambda y.y)(z)
 
endmodule