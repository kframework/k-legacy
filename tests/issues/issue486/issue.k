module ISSUE

syntax Exp ::= "pass"
             > "if" Bool "then" Exp "else" Exp

syntax K ::= "print"

rule if true then E else _ => E
rule if false then _ else E => E
rule pass => .
rule <k> print => . ...</k>
     <out>... . => ListItem("foo") </out>

configuration <T><k> if true then pass else pass ~> if true then pass else pass ~> print ~> print </k><out stream="stdout"> .List </out></T>

endmodule
