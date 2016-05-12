// Copyright (c) 2014-2016 K Team. All Rights Reserved.
requires "domains.k"

module UNPARSING
    syntax Exp ::= "x" | "y"
                 | "if" Exp "then" Exp "else" Exp [ unparse("if $1 then $2 else $3") ]
                 | r"[1-9]+"        [ token ]

    configuration <k> $PGM </k>

    rule x => y
endmodule
