require "string.k"

module ID-SYNTAX-HOOKS
  /*@ This is the default definition used for variable names. Because it's
   tagged with the notInRules tag, kompile does not use this definition when
   parsing rules in .k files; it's used only when running a file with a kompiled
   definition.
  */
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
