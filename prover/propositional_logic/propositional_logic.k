module PROPOSITIONAL_LOGIC
  import PROPOSITIONAL_LOGIC-SYNTAX

  rule A:Formula iff B:Formula => (A implies B) and (B implies A)
  rule A:Formula implies B:Formula => (not A) or B
endmodule

module PROPOSITIONAL_LOGIC-SYNTAX
  syntax Var ::= Id
  syntax Formula ::= Var
                   | "not" Formula
                   > Formula "and" Formula              [strict,left]
                   > Formula "or" Formula               [strict,left]
                   | Formula "implies" Formula          [strict, right]
                   | Formula "iff" Formula              [strict]
                   | "(" Formula ")"                    [bracket]

   syntax KResult ::= Var
                    | "not" KResult
                    | KResult "or" KResult
endmodule

