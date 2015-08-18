require "domains.k"

module WASM-SYNTAX
  imports INT-SYNTAX
  imports FLOAT-SYNTAX

  syntax Script ::= Commands

  syntax Commands ::= List{Command, ""}                         [klabel('__:Commands)]
  syntax Command  ::= Module
                    | Invoke

  syntax Module ::= "(" "module" ModuleItems ")"
  syntax Invoke ::= "(" "invoke" Var Exps ")"

  syntax ModuleItems ::= List{ModuleItem, ""}                   [klabel('__:ModuleItems)]
  syntax ModuleItem  ::= Function
                       | Global
                       | Export
                       | Table
                       | Memory

  syntax Function ::= "(" "func" Param Result Local Exps ")"
  syntax Param  ::= "(" "param" Types ")"
  syntax Result ::= "(" "result" Types ")"
  syntax Local  ::= "(" "local" Types ")"

  syntax Global ::= "(" "global" Types ")"
  syntax Export ::= "(" "export" Vars ")"
  syntax Table  ::= "(" "table" Vars ")"
  syntax Memory ::= "(" "memory" Int ")"

  syntax Exps ::= List{Exp, ""}                                 [seqstrict]
  syntax Exp  ::= "(" "nop" ")"
                | "(" "block" Exps ")"                          [strict]
                | "(" "if" Exp Exp Exp ")"                      [strict(1)]
                | "(" "if" Exp Exp ")"
                | "(" "loop" Exps ")"
                | "(" "label" Exps ")"
                | "(" "break" Var /* Exps */ ")"
                | "(" "break" ")"
                | "(" "switch" Exp Cases Exp ")"
                | "(" "call" Var Exps ")"                       [strict(2)]
                | "(" "dispatch" Var Exp Exps ")"
                | "(" "return" Exps ")"                         [strict(1)]
                | "(" "destruct" Vars Exp ")"                   [strict(2)]
                | "(" "getlocal" Var ")"
                | "(" "setlocal" Var Exp ")"                    [strict(2)]
                | "(" "getglobal" Var ")"
                | "(" "setglobal" Var Exp ")"                   [strict(2)]
                | "(" "get" MemOp "." MemType Exp ")"           [strict(3)]
                | "(" "set" MemOp "." MemType Exp Exp ")"       [strict(3,4)]
                | "(" "const" "." Type Literal ")"
                | "(" UnOp  "." Type Exp ")"                    [strict(3)]
                | "(" BinOp "." Type Exp Exp ")"                [seqstrict(3,4)]
                | "(" RelOp "." Type Exp Exp ")"                [seqstrict(3,4)]
                | "(" "convert" Sign "." Type "." Type Exp ")"
                | "(" "convert"      "." Type "." Type Exp ")"
                | "(" "cast"         "." Type "." Type Exp ")"
                | "(" "print" Exp ")"                           [strict]


  syntax Cases ::= List{Case, ""}                               [klabel('__:Cases)]
  syntax Case  ::= "(" "case" Literal Exps "fallthru" ")"
                 | "(" "case" Literal Exps            ")"
                 | "(" "case" Literal                 ")"

  syntax Types ::= List{Type, ""}                               [klabel('__:Types)]
  syntax Type  ::= "i32" | "i64" | "f32" | "f64"
  syntax MemType ::= Type | "i8" | "i16"

  syntax Vars ::= List{Var, ""}                                 [klabel('__:Vars)]
  syntax Var  ::= Int

  syntax Literal ::= Int | Float

  syntax UnOp  ::= "neg" | "abs"
                 | "not" | "clz" | "ctz"                        // int
                 | "ceil" | "floor" | "trunc" | "round"         // float
  syntax BinOp ::= "add" | "sub" | "mul"
                 | "divs" | "divu" | "mods" | "modu"            // int
                 | "and" | "or" | "xor" | "shl" | "shr" | "sar" // int
                 | "div" | "mod" | "copysign"                   // float
  syntax RelOp ::= "eq" | "neq"
                 | "lts" | "ltu" | "les" | "leu"                // int
                 | "gts" | "gtu" | "ges" | "geu"                // int
                 | "lt" | "le" | "gt" | "ge"                    // float
  syntax MemOp ::= Dist Align Sign
                 | Dist Align
                 | Dist       Sign
                 | Dist

  syntax Dist  ::= "near" | "far"
  syntax Align ::= "unaligned"
  syntax Sign  ::= "s" | "u"

  // macros for syntactic sugars
  rule (if E S) => (if E S (nop))                               [macro]
  rule (break) => (break 0)                                     [macro]

endmodule
