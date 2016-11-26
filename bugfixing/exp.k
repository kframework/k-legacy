module EXP-SYNTAX

  // grammar: BNF = Backus-Naur Form


    syntax Pgm ::= Int
    syntax Ids ::= List{Id,","}

endmodule

module EXP
  imports EXP-SYNTAX

  syntax KResult ::= Int | Bool

  configuration <H> <k> $PGM:Pgm </k> <state> .Map </state> </H>

  rule <H> <k> 1 </k> <state> .Map </state> </H>
    => <H> <k> 2 </k> <state> .Map </state> </H>


endmodule
