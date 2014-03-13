require "k-equal.k"
require "int.k"
require "k-prelude.k"

module SET
  imports K-EQUAL-HOOKS
  imports INT-HOOKS
  imports DEFAULT-CONFIGURATION

  /*@ \section{The K Set} The Set construct behaves like a mathematical
   set: it is a collection of unique items. The builtin \K Set offers support
   for some common set operations.
  */

  /*@ \section{Adding and removing from Sets}
   Currently, there are no explicit rules for adding to or removing from a Set.
   To add to a Set, match a Set on the left-hand side of a rule, and then mention
   it as part of the set in the right hand side. for instance: \\
   rule X S:MySet (. => MySetItem(X)) \\
   To remove an item, a similar procedure is used. Match the set with the item
   on the left-hand side of a rule, and eliminate the element in the right-hand
   side. For instance: \\
   rule S:MySet (X:MySetItem => .)
  */


  /*@ Construct a set from the union of two different sets (A $\cup$ B): */
  syntax MySet ::= MySet "," MySet    [left, function, hook(Set:__), klabel('_Set_)]
  /*@ Construct an empty Set: */
  syntax MySet ::= ".MySet"       [function, hook(Set:.Set)]
  /*@ TODO: I don't know what this does. Construct a set of one element? Is this
   used to add elements to Sets? I never figured out how to add elements to
   Sets in 422; I actually once used Lists just to avoid having to. */
  syntax MySet ::= MySetItem(K)   [function, hook(Set:SetItem)]

  /*@ Get the difference of two sets: */
  syntax MySet ::= MySet "-MySet" MySet   [function, hook(Set:difference), latex({#1}-_{\it Set}{#2})]
//  rule (MySetItem(K:K) S1:MySet) -MySet S2:MySet => S1 -MySet S2
//  when K in S2
//  rule (MySetItem(K:K) S1:MySet) -MySet S2:MySet => MySetItem(K) (S1 -MySet S2)
//  when notBool(K in S2)
//  rule .MySet -MySet _:MySet => .MySet

  /* set difference axioms */
//  rule S1:MySet -MySet (MySetItem(K:K) S2:MySet) => S1 -MySet S2
//  when notBool(K in S1)
//  rule S:MySet -MySet .MySet => S

  /*@ Construct a Set consisting of the intersection of two sets (A $\cap$ B):*/
  syntax MySet ::= intersectSet(MySet, MySet)   [function]
//  rule intersectSet(S1:MySet, S2:MySet) => S1 -MySet (S1 -MySet S2)

  /*@ \textbf{NOTE:} Some convenient operations between Sets (inclusion, equality, inequality, etc.)
   are defined in \hyperref[sec:K-EQUAL-HOOKS]{K-EQUAL-HOOKS}.*/

  /*@ Test element membership in a set ( a $\epsilon$ A ) :*/
  syntax Bool ::= K "in" MySet    [function, hook(Set:in)]
//  rule K1:K in MySetItem(K2:K) S:MySet => K1 =K K2 orBool K1 in S
//  rule _ in .MySet => false

  /*@ Get the cardinality of a set (|A|) :*/
  syntax Int ::= size(MySet)   [function]
//  rule size(MySetItem(_) S:MySet) => 1 +Int size(S)
//  rule size(.MySet) => 0
endmodule

