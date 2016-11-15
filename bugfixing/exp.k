module EXP-SYNTAX

  // grammar: BNF = Backus-Naur Form

  syntax Exp ::= Int
               | Exp "+" Exp [left, strict]
               | "(" Exp ")" [bracket]

endmodule

module EXP
  imports EXP-SYNTAX

  syntax KResult ::= Int

  configuration <T>
                  <k> $PGM </k>
                </T>

  rul <T> <k> I1:Int + I2:Int </k> </T>
      =>
      <T> <k> I1 +Int I2 </k> </T>

endmodule
