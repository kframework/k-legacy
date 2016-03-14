// Copyright (c) 2014-2016 K Team. All Rights Reserved.
requires "domains.k"

module EXP
    syntax Exp
endmodule

module A
    import EXP
    syntax Exp ::= "x" | "y"
    configuration <k> $PGM </k>

    rule x => y
endmodule

module B
    import EXP
    syntax Exp ::= "foo" | "bar"
    configuration <b> foo </b>

    rule <b> foo => bar </b>
endmodule

module C
    imports A
    imports B

    configuration <t> initKCell(Init) initBCell </t>

    rule <k> y => foo ...</k> <b> bar => x </b>
endmodule

module D
    imports A
    imports B

    configuration <t> initKCell(Init) initBCell <c> initBCell </c> </t>

    rule <k> y => foo ...</k> <b> bar => x </b>
endmodule
