require "domains.k"

module COOL
  imports INT
  syntax S ::= S "*" S    [assoc, unit(.S)]
             | ".S"
             | "a"
             | "b"
  syntax KItem ::= foo(S)
  rule <k> 0 => foo(`b * b` * `a * b`) </k>
  rule <k> foo(`X * a` * Y) => 1 </k>

  configuration <k> 0 </k>
endmodule