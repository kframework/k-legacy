// Copyright (c) 2012-2016 K Team. All Rights Reserved.

[ #MainModule(LAMBDA)
, #EntryModules(LAMBDA)
]

module SUBSTITUTION
  //imports MAP

  // used for user-defined substitution only
  syntax KVariable
  syntax Map

  syntax K ::= K "[" K "/" K "]"  [function, hook(SUBSTITUTION.userSingletonSubstitutionKore), impure]
  syntax K ::= K "[" Map "]"      [function, hook(SUBSTITUTION.userSubstitutionKore), impure]
endmodule


module LAMBDA
  imports SUBSTITUTION

  syntax Id ::= "a" | "b" | "c"
  syntax Val ::= Id
               | "lambda" Id "." Exp  [binder]
  syntax Exp ::= //Val
               Exp Exp              [strict, left]
               | "(" Exp ")"          [bracket]
  syntax KVariable ::= Id
  syntax KResult ::= Val

//  rule (lambda X:Id . E:Exp) V:Val => E[V / X]

  syntax Int ::= "1" | "2" | "3"
  syntax Bool ::= "true" | "false"
  syntax Val ::= Int | Bool
  syntax Exp ::= Exp "*" Exp          [strict, left]
               | Exp "/" Exp          [strict]
               | Exp "+" Exp          [strict, left]
               | Exp "<=" Exp         [strict]

//  rule E1:Exp + E2:Exp => E1:Exp
//  rule I1 * I2 => I1 *Int I2
//  rule I1 / I2 => I1 /Int I2
//  rule I1 + I2 => I1 +Int I2
//  rule I1 <= I2 => I1 <=Int I2

  syntax Exp ::= "if" Exp "then" Exp "else" Exp  [strict(1)]
//  rule if true  then E1:Exp else E2:Exp => E1:Exp
//  rule if false then _ else E => E

  syntax Exp ::= "let" Id "=" Exp "in" Exp
//  rule let X = E in E':Exp => (lambda X . E') E                        [macro]

  syntax Exp ::= "letrec" Id Id "=" Exp "in" Exp
               | "mu" Id "." Exp      [binder]
//  rule letrec F:Id X = E in E' => let F = mu F . lambda X . E in E'    [macro]
//  rule mu X . E => E[(mu X . E) / X]
endmodule
