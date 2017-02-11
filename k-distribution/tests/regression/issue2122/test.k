module TEST-SYNTAX

  // grammar: BNF = Backus-Naur Form
  syntax Exp ::= Int
               | Exp "+" Exp [left, strict]
               | "(" Exp ")" [bracket]

endmodule

module TEST
  imports TEST-SYNTAX

  syntax KResult ::= Int

  configuration <T>
                  <k> $PGM:Exp </k>
                  <state> .Map </state>
                </T>

  rule <T> <k> I1:Int + I2:Int </k> </T>
       =>
       <T> <k> I1 +Int I2 </k> </T>

endmodule