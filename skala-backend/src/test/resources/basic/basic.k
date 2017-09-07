module BASIC-SYNTAX
    syntax Exp ::= Int
                   | Exp "+" Exp
endmodule
module BASIC
    imports BASIC-SYNTAX
    configuration <T> <k> $PGM:Exp </k> </T>
    rule A:Int + B:Int => A +Int B
endmodule