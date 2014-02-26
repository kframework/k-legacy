require "string.k"

module ID-SYNTAX-HOOKS
  /*@ This is the default definition used for variable names.*/
  syntax Id ::= Token{[A-Za-z\_][A-Za-z0-9\_]*}   [notInRules]
endmodule

module ID-HOOKS
  imports ID-SYNTAX-HOOKS
  imports STRING-HOOKS
  imports K


  /*@ cast an Id to a String:*/
  syntax String ::= "Id2String" "(" Id ")"		 [function, hook(#CONVERSION:id2string), klabel(Id2String)]
  /*@ cast a String to an Id:*/
  syntax Id ::= "String2Id" "(" String ")"		 [function, klabel(String2Id)]

  rule Id2String(X:Id) => #tokenToString(X)
  rule String2Id(S:String) => #parseToken("Id", S)
endmodule
