module IMP-SPEC
imports IMP

rule
<T>
<L1>
  <k>
      sum = sum + n;
      n = n + 1;
      n = n + 1;
  =>
    .K
  ...</k>
  <state>...
    sum |-> (S:Int => ?S1:Int)
    n   |-> (N:Int => ?N1:Int)
  ...</state>
</L1>
<L2>
  <k>
      sum = sum + n;
      n = n + 2;
  =>
    .K
  ...</k>
  <state>...
    sum |-> (S:Int => ?S2:Int)
    n   |-> (N:Int => ?N2:Int)
  ...</state>
</L2>
</T>
requires N >=Int 0
ensures ?N1 ==K ?N2
andBool ?S1 ==K ?S2

endmodule
