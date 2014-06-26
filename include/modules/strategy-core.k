// Copyright (c) 2014-2014 K Team. All Rights Reserved.

require "modules/k-functional-visitor.k"

module SUBSTITUTION 
 imports K-FUNCTIONAL-VISITOR

 syntax Replace ::= K "[" K "|" K "]"                        [function]

 syntax KItem ::= replaceThisWithThat(K, K)                  [function]
 rule replaceThisWithThat(X, Y) => Y
 
 rule X:K [ Y:K | Z:K ] => #visit(X, 'replaceThisWithThat, Y, '_==K_, Z) 
endmodule

module STRATEGY-CORE
 imports K-FUNCTIONAL-VISITOR
 imports SUBSTITUTION

 syntax Bool ::= "#stuck"  [function, hook(#STUCK:#stuck)]

 // manhole... just kidding, hole with memoization
 syntax MHole ::= MHOLE(K)

 // the hole used in the strategy language
 syntax SHole ::= "SHOLE"
 
 syntax Strategy ::= Id | Replace | SHole

 // the visitors
 syntax KItem ::= replaceHoleWithCurrent(K, K)            [function]
 rule replaceHoleWithCurrent(_, X) => X
 syntax KItem ::= replaceHoleWithPrevious(K)              [function]
 rule replaceHoleWithPrevious(MHOLE(X)) => X
 
 syntax Bool ::= ieMHole(K)                               [function]
 rule ieMHole(MHOLE(_)) => true      
endmodule
