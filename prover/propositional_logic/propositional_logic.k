module PROPOSITIONAL_LOGIC
  import PROPOSITIONAL_LOGIC-SYNTAX

  rule A:Formula iff B:Formula => (A implies B) and (B implies A)
endmodule

module PROPOSITIONAL_LOGIC-SYNTAX
  syntax Var ::= Id
  syntax Formula ::= Var
                   | "not" Formula
                   > Formula "and" Formula              [left]
                   > Formula "or" Formula               [left]
                   | Formula "implies" Formula          [strict, right]
                   | Formula "iff" Formula              [strict]
                   | "(" Formula ")"                    [bracket]
endmodule

