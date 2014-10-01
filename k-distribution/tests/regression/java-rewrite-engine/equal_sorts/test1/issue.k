// Copyright (c) 2013-2014 K Team. All Rights Reserved.

module ISSUE

  syntax Sort ::= Int | Bool
  syntax KResult ::= Int | Bool
  syntax KItem ::= "test"

  rule test => isKResult(?S:Sort) ~> isKResult(?K:KResult) ~> isSort(?S:Sort) ~> isSort(?K:KResult)
  rule <k> true ~> R </k> => <k> R </k>

endmodule
