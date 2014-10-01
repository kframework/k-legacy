// Copyright (c) 2013-2014 K Team. All Rights Reserved.

module ISSUE

  syntax S1 ::= Int
  syntax KResult ::= S1
  syntax KItem ::= "test"

  rule test => isS1(?A:KResult) ~> isKResult(?B:S1)
  rule <k> true ~> R </k> => <k> R </k>

endmodule
