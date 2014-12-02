// Copyright (c) 2012-2014 K Team. All Rights Reserved.
require "builtins/bool.k"
require "builtins/k-equal.k"
module UNIFICATION
  imports BOOL-HOOKS
  imports K-EQUAL-HOOKS

//@ Public interface to the unification procedure (what the user needs to know)
  syntax Mgu ::= ".Mgu"  [constructor, hook(Unification:emptyMgu)]
  rule .Mgu => subst(.KList)  [structural, anywhere]  // empty mgu
  rule isMgu(subst(_)) => true  [function]  // so we can write Theta:Mgu

  syntax Mgu ::= updateMgu(Mgu,K,K)  [function, hook(Unification:updateMgu)]  // updates Mgu with equation
  syntax K ::= applyMgu(Mgu,K)       [function, hook(Unification:applyMgu), latex({#1}({#2}))]  // applies Mgu to any term

/* If the user's language supports unification against don't-care terms
    (e.g. _ in Prolog), they should declare some syntax like the following:
syntax ThingThatCanBeUnified ::= "_" [onlyLabel, klabel($dontcare)]
*/
  syntax KItem ::= "$dontcare" [klabel($dontcare), latex(\AnyVar[K])]

// Traian: I would remove applySubst all together and only keep applyMgu
//         Mgu gives a nicer, more intuitive user interface than Subst

//@ Cleanup, decomposition and orientation of equations.

  rule eqns((eqn(K:K,,K) => .KList),,_)   [structural, anywhere]

/*@ Generic rule for decomposing the constraints.
Matching on any KAST label, the
*/
  rule eqns((eqn(K:K,,$dontcare) => .KList),,_) [structural, anywhere]
  rule eqns((eqn($dontcare,,K:K) => .KList),,_) [structural, anywhere]


/*@ When an equation contains a variable, it can be added as a binding
to the mgu.  While we know that the variables in the domain of the mgu
do not appear anywhere in the equations, it can be and usually is the
case that variables appearing in the equations also appear in the
codomain of the mgu.  Thus, we have to refine the mgu by applying the
new binding to all its terms.  Moreover, to maintain the equations
canonical in terms of the current mgu, we also have to refine the
remaining equations by applying the new mgu binding to them. */


/*@ Final rule of the unification algorithm, retrieving the substitution
once all constraints were incorporated.
*/
  rule unifyEqs(subst(Eqns:KList),,eqns(.KList)) => subst(Eqns)
   [structural, anywhere]
/*@ the subTerm relation is used to determine cyclic uunification */
  syntax Bool ::= K "subTerm" K [function]
  rule K1 subTerm K2 => (K1 ==K K2) orElseBool (K1 properSubTerm K2)
    [structural, anywhere]
  syntax Bool ::= K "properSubTerm" K [function]
  rule K1 properSubTerm L:KLabel(.KList) => false   [structural, anywhere]
  rule K1:K properSubTerm L:KLabel(K2:K,,Ks:KList)
      => (K1 subTerm K2) orElseBool (K1 properSubTerm L:KLabel(Ks))
   [structural, anywhere]
/*@ Generic first order substitution on K ASTs.
Note that ~> is not handled  and neither are binding operations.
*/

  rule substitute(X:K,,K:K,,.KList) => .KList   [structural, anywhere]
  rule substitute(X:K,,K:K,,K1:K,,K2:K,,Ks:KList)
    => substitute(X,,K,,K1),,substitute(X,,K,,K2),,substitute(X,,K,,Ks)
    [structural, anywhere]
  rule substitute(X:K,,K:K,,X) => K   [structural, anywhere]

  //TODO:  Consider implementing substitute for other collections, too

  syntax K ::= "unify" "(" K "," K ")" [function]
  rule unify(K1, K2) => unifyEqs(subst(.KList),,eqns(eqn(K1,,K2)))
   [structural, anywhere]
/*@ updateMgu adds a constraint cu the current MGU substitution.
The existing mgu is first applied to the constraint being introduced to
guarantee that the variables in the domain of the mgu don't appear in the
constraint.
*/

  syntax K ::= applySubst(K, K) [function]
  rule applySubst(subst(.KList),K) => K   [structural, anywhere]

  syntax KLabel ::= "unifyEqs" | "subst" | "#vars" | "substitute"
                  | "eqn" | "eqns" | "mgu"
                  | "getVars" | "accumulateVars"
                  | "clash" | "cycle"

  syntax MguError
  syntax MguOrError ::= Mgu | MguError
  rule isMguError(clash(_)) => true
  rule isMguError(cycle(_)) => true

  rule getVars(K) => accumulateVars(#vars(.KList),, K)
    [structural, anywhere]

  rule accumulateVars(#vars(Xs:KList)) => #vars(Xs)   [structural, anywhere]


  syntax Map ::= #metaVariablesMap(K) [function]
  rule #metaVariablesMap(K:K) => #metaVarsMap(getVars(K), .Map)

  syntax Map ::= #metaVarsMap(K, Map) [function]
  rule #metaVarsMap(#vars(.KList), M:Map) => M:Map

//  configuration <k> .K </k> <nextVar> 0 </nextVar>

  syntax K ::= getAlpha(K)  [function]
  rule getAlpha(K)
    => freshSubst(subst(.KList),, getVars(K))

  syntax K ::= freshSubst(KList)  [function]

  rule freshSubst(subst(Eqns:KList),,#vars(.KList)) => subst(Eqns)





// alpha conversion moved here by grigore
 syntax K ::= renameVariables(K)  [function, hook(MetaK:renameVariables), impure]
//            | applyAlpha(K)

// rule renameVariables(K) => getAlpha(K) ~> applyAlpha(K)
// rule subst(Eqns:KList) ~> applyAlpha(K) => applySubst(subst(Eqns),K)
 rule renameVariables(K) => applySubst(getAlpha(K),K)

// Turns an Mgu into a Map, ignoring the symbolic bindings
  syntax Map ::= Mgu2Map(K) [function]
  rule Mgu2Map(K) => Mgu2MapHelper(K, .Map)

  syntax Map ::= Mgu2MapHelper(K, Map) [function]

/*
  rule Mgu2Map(subst((eqn(K1:K,,K2:K) => eqn(K2:K,,K1:K)),,_))
    when isSymbolicK(K1)
    andBool isVariable(K2)
    andBool isSymbolicK(K2) =/=K true
// reorient symbolic-to-concrete variable bindings

  rule Mgu2Map(subst((eqn(K1:K,,K2:K) => .KList),,_))
    when isSymbolicK(K1)
    andBool isSymbolicK(K2)
  rule Mgu2Map(subst((eqn(K1:K,,K2:K) => .KList),,_))
    when isSymbolicK(K1)
    andBool (isVariable(K2) =/=K true)
  // ignore symbolic bindings
  rule Mgu2Map(subst((eqn(K1:K,,K2:K) => .KList),,_))
       (. => K1 |-> K2)
    when isSymbolicK(K1) =/=K true

*/


  rule Mgu2MapHelper(subst(.KList), M:Map) => M



endmodule
