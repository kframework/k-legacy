// Copyright (c) 2015 K Team. All Rights Reserved.
require "domains.k"
require "wasm-syntax.k"

module WASM

  imports INT
  imports FLOAT
  imports BASIC-K
  imports LIST
  imports MAP
  imports SET
  imports STDOUT-STREAM
  imports MINT
  imports WASM-SYNTAX

  configuration <T>
    <k> $PGM:Script:K </k>
    <mem> .Map </mem>
    <func>
      <funcTbl> .Map </funcTbl>
      <funcIdx> 0 </funcIdx>
    </func>
    <ctx>
      <storage>
        <local> .Map </local>
        <global>.Map </global>
      </storage>
      <callStack> .List </callStack>
      <labelStack> .List </labelStack>
    </ctx>
    <stdout />
  </T>

  syntax Val  ::= Literal
                | tuple(Vals)
                | "unit"
  syntax Vals ::= List{Val, ""}
  syntax Exp  ::= Val
  syntax Exps ::= Vals
  syntax KResult ::= Val | Vals

  // Commands => .K

  rule C Cs:Commands => C ~> Cs
  rule .Commands => .K

  // Command => .K

  rule (module MIs) => MIs

  rule (invoke X Es) => (print (call X Es))

  // ModuleItems => .K

  rule MI MIs:ModuleItems => MI ~> MIs
  rule .ModuleItems => .K

  // ModuleItem => .K

  rule <k> (func P R L S) => .K ... </k>
       <funcTbl> .Map => I |-> (func P R L S) ... </funcTbl>
       <funcIdx> I => I +Int 1 </funcIdx>

  rule <k> (global Ts) => .K ... </k>
       <global> .Map => initGlobal(Ts,0) </global>
  //
  syntax Map ::= initGlobal(Types,Int) [function]
  rule initGlobal(T Ts, I) => I |-> defaultValue(T) initGlobal(Ts, I +Int 1)
  rule initGlobal(.Types, _) => .Map
  //
  syntax Val ::= defaultValue(Type) [function]
  rule defaultValue(i32) => 0

  rule (export _) => .K // TODO: implement properly

  // TODO: rule (table ...) => ...

  rule <k> (memory `N => N -Int 1`) ... </k>
       <mem> .Map => N -Int 1 |-> mi(8,0) ... </mem>
       requires N >Int 0
  rule (memory 0) => .K

  // Exps => Vals
  // Exp => Val

  rule (nop) => unit

  rule `(block_)`(`__`(_:Val,Vs:Vals) => Vs) requires Vs =/=K .Vals
  rule `(block_)`(`__`(V:Val,.Vals)) => V

  rule (if V:Val S _) => S requires V =/=K 0
  rule (if 0     _ S) => S

  rule (loop Ss) => `(block_)`(`__`((block Ss), `__`((loop Ss), .Exps)))

  rule <k> (label Ss) ~> K => (block Ss) ~> popLabel </k>
       <labelStack> .List => ListItem(K) ... </labelStack>
  rule <k> V:Val ~> `popLabel ~> _ => K` </k>
       <labelStack> ListItem(K) => .List ... </labelStack>
  syntax KItem ::= "popLabel"

  rule <k> (break 0) ~> _ => unit ~> K </k>
       <labelStack> ListItem(K) => .List ... </labelStack>
  rule <k> (break `X:Int => X -Int 1`) ... </k>
       <labelStack> ListItem(_) => .List ... </labelStack>
       requires X >Int 0

  // TODO: rule (switch ...

  rule <k> `(call__)`(X,Vs:Vals) ~> K => `(return_)`(`__`((block Ss), .Exps)) </k>
       <funcTbl> ... X |-> (func _ _ (local Ls) Ss) ... </funcTbl>
       <callStack> .List => ListItem((Local,K)) ... </callStack>
       <local> Local => initLocal(Vs,Ls,0) </local>
  //
  syntax Map ::= initLocal(Vals,Types,Int) [function]
  rule initLocal(V Vs, Ts, I) => I |-> V initLocal(Vs, Ts, I +Int 1)
  rule initLocal(.Vals, T Ts, I) => I |-> defaultValue(T) initLocal(.Vals, Ts, I +Int 1)
  rule initLocal(.Vals, .Types, _) => .Map
  //
  syntax KItem ::= "(" Map "," K ")"

  // TODO: rule (dispatch ...

  rule <k> `(return_)`(Vs:Vals) ~> _ => retval(Vs) ~> K </k>
       <callStack> ListItem((Local,K)) => .List ... </callStack>
       <local> _ => Local </local>
  //
  syntax Val ::= retval(Vals) [function]
  rule retval(.Vals) => unit
  rule retval(V Vs) => V           requires Vs ==K .Vals
  rule retval(V Vs) => tuple(V Vs) requires Vs =/=K .Vals

  rule (destruct `X Xs` tuple(V Vs)) => `(block_)`(`__`((setlocal X V), `__`((destruct Xs tuple(Vs)), .Exps)))
  rule (destruct .Vars tuple(.Vals)) => unit

  rule <k> (getlocal X) => V ... </k>
       <local> ... X |-> V ... </local>

  rule <k> (setlocal X V:Val) => unit ... </k>
       <local> ... X |-> `_ => V` ... </local>

  rule <k> (getglobal X) => V ... </k>
       <global> ... X |-> V ... </global>

  rule <k> (setglobal X V:Val) => unit ... </k>
       <global> ... X |-> `_ => V` ... </global>

  rule (getnears.T L:Int) => (getnearunaligneds.T L)
       requires L %Int byteSize(T) ==K 0

  rule (setnears.T L:Int I:Int) => (setnearunaligneds.T L I)
       requires L %Int byteSize(T) ==K 0

  syntax Int ::= byteSize(MemType) [function]
  rule byteSize(i8) => 1
  rule byteSize(i16) => 2
  rule byteSize(i32) => 4
  rule byteSize(i64) => 8
  rule byteSize(f32) => 4
  rule byteSize(f64) => 8

  rule <k> (getnearunaligneds.i32 L:Int)
        => svalueMInt(concatenateMInt(I0, concatenateMInt(I1, concatenateMInt(I2,I3)))) ... </k>
       <mem>
         ...
         L        |-> I0
         L +Int 1 |-> I1
         L +Int 2 |-> I2
         L +Int 3 |-> I3
         ...
       </mem>

  rule <k> (setnearunaligneds.i32 L:Int I:Int) => unit ... </k>
       <mem>
         ...
         L        |-> `_ => extractMInt(mi(32,I),  0,  8)`
         L +Int 1 |-> `_ => extractMInt(mi(32,I),  8, 16)`
         L +Int 2 |-> `_ => extractMInt(mi(32,I), 16, 24)`
         L +Int 3 |-> `_ => extractMInt(mi(32,I), 24, 32)`
         ...
       </mem>

  rule (const.i32 I:Int) => I

  rule (add.i32 I1:Int I2:Int) => I1 +Int I2
  rule (sub.i32 I1:Int I2:Int) => I1 -Int I2
  rule (mul.i32 I1:Int I2:Int) => I1 *Int I2

  rule ( eq.i32 I1:Int I2:Int) => bool2int(I1 ==K I2) // ==Int ?
  rule (neq.i32 I1:Int I2:Int) => bool2int(I1 =/=K I2) // =/=Int ?
  //
  syntax Int ::= bool2int(Bool) [function]
  rule bool2int(true) => 1
  rule bool2int(false) => 0

  // TODO: rule (convert ...

  // TODO: rule (cast ...

  rule <k> (print I:Int) => .K ... </k>
       <stdout> ... .List => ListItem(I) ListItem("\n") </stdout>

endmodule
