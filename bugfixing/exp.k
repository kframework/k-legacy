module EXP-SYNTAX

  // grammar: BNF = Backus-Naur Form

  syntax Exp ::= Int

endmodule

module EXP
  imports EXP-SYNTAX

  syntax KResult ::= Int

  configuration <T> <k> $PGM </k> </T>

  rule <T> <k> I1:Int </k> </T>
      =>
      <T> <k> 1 </k> </T>

endmodule
