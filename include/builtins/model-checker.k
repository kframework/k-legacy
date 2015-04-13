// Copyright (c) 2012-2014 K Team. All Rights Reserved.
require "bool.k"

module LTL-SYNTAX-HOOKS
  imports #LTL-INTERFACE
  syntax #LtlFormula ::= "TrueLtl"
  syntax #LtlFormula ::= "FalseLtl"

  syntax LtlFormula ::= #LtlFormula

endmodule

module LTL-HOOKS
  imports LTL-SYNTAX-HOOKS

  syntax LtlFormula ::= "~Ltl" LtlFormula [function,
                                           latex(\kBuiltinOp{\sim}{Ltl} {#1})]
                     |  "OLtl" LtlFormula [function,
                                           latex(\kBuiltinOp{\mathsf{O}}{Ltl} {#1})]
                     |  "<>Ltl" LtlFormula [function,
                                            latex(\kBuiltinOp{\diamond}{Ltl} {#1})]
                     |  "[]Ltl" LtlFormula [function,
                                            latex(\kBuiltinOp{\square}{Ltl} {#1})]
                     | "(" LtlFormula ")" [bracket]
                     >  LtlFormula "/\\Ltl" LtlFormula [function, left,
                                                        latex({#1}\kBuiltinOp{\land}{Ltl} {#2})]
                     >  LtlFormula "\\/Ltl" LtlFormula [function, left,
                                                        latex({#1}\kBuiltinOp{\lor}{Ltl} {#2})]
                     > left:
                        LtlFormula "ULtl" LtlFormula  [function, left,
                                                       latex({#1}\kBuiltinOp{\mathsf{U}}{Ltl} {#2})]
                     |  LtlFormula "RLtl" LtlFormula  [function, left,
                                                       latex({#1}\kBuiltinOp{\mathsf{R}}{Ltl} {#2})]
                     |  LtlFormula "WLtl" LtlFormula  [function, left, 
                                                       latex({#1}\kBuiltinOp{\mathsf{W}}{Ltl} {#2})]
                     |  LtlFormula "|->Ltl" LtlFormula [function, left,
                                                        latex({#1}\kBuiltinOp{\mapsto}{Ltl} {#2})]
                     > left:
                        LtlFormula "->Ltl" LtlFormula  [function, left,
                                                        latex({#1}\kBuiltinOp{\right{arrow}}{Ltl} {#2})]
                     |  LtlFormula "<->Ltl" LtlFormula [function, left,
                                                        latex({#1}\kBuiltinOp{\leftrightarrow}{Ltl} {#2})]
                     |  LtlFormula "=>Ltl" LtlFormula  [function, left,
                                                        latex({#1}\kBuiltinOp{\Rightarrow}{Ltl} {#2})]
                     |  LtlFormula "<=>Ltl" LtlFormula [function, left,
                                                        latex({#1}\kBuiltinOp{\Leftightarrow}{Ltl} {#2})]
endmodule

module MODEL-CHECKER-SYNTAX-HOOKS
  imports LTL-SYNTAX-HOOKS
  syntax #ModelCheckerState
  syntax ModelCheckerState ::= #ModelCheckerState 
  syntax #Prop
  syntax Prop ::= #Prop
  syntax #ModelCheckResult
  syntax ModelCheckResult ::= #ModelCheckResult
  syntax LtlFormula ::= Prop
endmodule


module  MODEL-CHECKER-INTERFACE
  imports #MODEL-CHECKER-INTERFACE

  syntax Bool ::= Bag "|=Ltl" K [latex({#1}\kBuiltinOp{\models}{Ltl} {#2})]

endmodule

module MODEL-CHECKER-HOOKS
  imports MODEL-CHECKER-INTERFACE
  imports MODEL-CHECKER-SYNTAX-HOOKS
  imports BOOL-HOOKS

  syntax ModelCheckResult ::= "modelCheck" "(" ModelCheckerState "," LtlFormula ")" 


endmodule
