// Copyright (c) 2012-2014 K Team. All Rights Reserved.
require "k-prelude.k"

module SUBSTITUTION
  imports MAP

  syntax K ::= K "[" K "/" K "]"  [function, hook(Substitution:userSubstitution), impure]
  syntax K ::= K "[" Map "]" [function]
  syntax Set ::= freeVariables(KList)  [function, hook(Substitution:freeVariables), latex({\it FV}({#1})), impure]
  syntax K ::= freshVariables(Set, K) [function, hook(MetaK:freshVariables), impure]

endmodule
