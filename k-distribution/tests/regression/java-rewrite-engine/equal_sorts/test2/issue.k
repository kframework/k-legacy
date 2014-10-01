module TEST

  syntax S1 ::= Int
  syntax S2 ::= Bool | S1
  syntax KResult ::= S2
  syntax S3 ::= Int | Bool
  syntax KItem ::= "test"

   rule test =>
     isS3(?S:S3) ~>
     isS3(?K:KResult) ~>
     isKResult(?S:S3) ~>
     isKResult(?K:KResult)
   rule <k> true ~> R </k> => <k> R </k>

endmodule
