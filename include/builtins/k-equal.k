require "bool.k"
require "k-prelude.k"
module K-EQUAL-HOOKS
  imports #K-EQUAL-INTERFACE
  imports BOOL-HOOKS
  imports K-SET-EXTRAS
  imports K-MAP-EXTRAS

  /*@\section{Equality}
   This module defines common helper functions used to test equality, 
   inequality, and inclusion between some builtin \K constructs.
  */


  /*@ These rules are a blanket that cover anything not explicitly defined in this module.*/
  syntax Bool ::= left:
  				  K "==K" K           [function, hook(#K-EQUAL:_==K_), latex({#1}\mathrel{=_K}{#2}), equalEqualK]
				| K "=/=K" K          [function, hook(#K-EQUAL:_=/=K_), latex({#1}\mathrel{\neq_K}{#2}), notEqualEqualK]

  syntax priorities equalEqualK notEqualEqualK > boolOperation
  rule K1:K =/=K K2:K => notBool(K1 ==K K2)

  syntax K ::= "#if" Bool "#then" K "#else" K "#fi"   [nodoc, function, hook(KEqual:ite)]
  syntax KItem ::= "#if" Bool "#then" KItem "#else" KItem "#fi"   [nodoc, function, hook(KEqual:ite)]
  syntax Int ::= "#if" Bool "#then" Int "#else" Int "#fi"   [nodoc, function, smtlib(ite)]
  rule #if true #then K:K #else _ #fi => K
  rule #if false #then _ #else K:K #fi => K

  /*@ inclusion between sets */
  syntax Bool ::= Set "<=Set" Set    [function]
  rule SetItem(K:K) Set1:Set <=Set SetItem(K) Set2:Set => Set1 <=Set Set2
  rule .Set <=Set _:Set => true
  rule SetItem(K:K) _:Set <=Set Set2:Set => false
                            when notBool (K in Set2:Set)
  /*@ equality over sets */
  syntax Bool ::= Set "==Set" Set    [function]
  rule Set1:Set ==Set Set2:Set => (Set1 <=Set Set2) andBool (Set2 <=Set Set1)

  /*@ inequality over sets */
  syntax Bool ::= Set "=/=Set" Set   [function]
  rule Set1:Set =/=Set Set2:Set => notBool(Set1 ==Set Set2)

  /*@ inclusion between maps */
  syntax Bool ::= Map "<=Map" Map    [function]

  rule (MapItem1:MapItem Map1:Map) <=Map (MapItem1 Map2:Map)
        =>
       (Map1 <=Map Map2)


  rule .Map <=Map _:Map => true [function, anywhere]

  rule ((K1:K |-> K2:K) _:Map) <=Map Map2 => false
       when notBool $hasMapping(Map2, K1)
       orElseBool (K2 =/=K Map2:Map(K1))


  /*@ equality over maps */
  syntax Bool ::= Map "==Map" Map    [function]

  rule Map1:Map ==Map Map2:Map
       =>
      (Map1 <=Map Map2) andBool (Map2 <=Map Map1)


  /*@ inequality over maps */
  syntax Bool ::= Map "=/=Map" Map   [function]
  rule Map1:Map =/=Map Map2:Map => notBool(Map1 ==Map Map2)


  /*@ equality over KList */
  syntax Bool ::= KList "==KList" KList    [function]

  /*@ TODO: What's this? Is it important? */
  syntax K ::= dummy(KList)
  rule LK1:KList ==KList LK2:KList => dummy(LK1) ==K dummy(LK2)

  /*@ inequality over KList */
  syntax Bool ::= KList "=/=KList" KList   [function]
  rule LK1:KList =/=KList LK2:KList  => notBool(LK1 ==KList LK2)

  /*@ equality over KLabel */
  syntax Bool ::= KLabel "==KLabel" KLabel
    [function, hook(KEqual:_==KLabel_), latex({#1}\mathrel{=_{\it KLabel}}{#2})]
  rule '_==KLabel_(K1:K,,K2:K) => K1 ==K K2 [function, anywhere]

  /*@ inequality over KLabel */
  syntax Bool ::= KLabel "=/=KLabel" KLabel
    [function, hook(KEqual:_=/=KLabel_), latex({#1}\mathrel{\neq_{\it KLabel}}{#2})]
  rule Lbl1:KLabel =/=KLabel Lbl2:KLabel => notBool(Lbl1 ==KLabel Lbl2)

  /*@ equality over lists */
  syntax Bool ::= List "==List" List [function]
  rule ListItem(K1:K) L1:List ==List ListItem(K2:K) L2:List
       =>
       K1 ==K K2 andBool L1 ==List L2
  rule .List ==List ListItem(_:K) _:List => false
  rule ListItem(_:K) _:List ==List .List => false
  rule .List ==List .List => true

  /*@ inequality over lists */
  syntax Bool ::= List "=/=List" List [function]
  rule L1:List =/=List L2:List => notBool(L1 ==List L2)

  /*@ These declarations define ``=" as an alias for ``==" for the relevant types.*/

  syntax Bool ::= K "=K" K                  [function]
                | List "=List" List         [function]
                | Set "=Set" Set            [function]
                | Bag "=Bag" Bag            [function]
                | Map "=Map" Map            [function]
                | KList "=KList" KList      [function]
                | KLabel "=KLabel" KLabel   [function]

  rule K:K =K K => true

  rule ((K1:K,,KL1:KList) =KList (K2:K,,KL2:KList)) => ((K1 =K K2) andBool (KL1 =KList KL2))
  rule .KList =KList .KList => true
  rule (_:K,,_:KList) =KList .KList => false
  rule .KList =KList (_:K,,_:KList) => false
endmodule
