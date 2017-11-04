require "domains.k"

module IOTEST-SYNTAX
    syntax Tok ::= "a" | "b" | "c"
endmodule

module IOTEST
    imports IOTEST-SYNTAX

    configuration <k> $PGM:Tok</k>

    rule a => b

    rule b => c
endmodule

