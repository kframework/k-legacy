kmod SSRISC-CONFIGURATION is including SSRISC-TIME-MODEL + SSRISC-COMMUNICATION-INTERFACE + SSRISC-SYNTAX + PL-CONVERSION + K

syntax K ::= Instr | Reg | FReg 

syntax KResult ::= Int32 | Float 

configuration

    <k>.K</k>
    <pc>0</pc>
    <hi>0</hi>
    <lo>0</lo>
    <regs>.Map</regs>
    <break>0</break>
---    <fregs>.Map</fregs>


endkm
 
kmod SSRISC-SEMANTICS is including SSRISC-CONFIGURATION

rule nextReg(v0) => v1 [structural]

rule <k> ovf(V1:Int32,V2:Int32) => break; <_/k>
     if ovf32(V1,V2) ==Int 1

rule <k> ovf(V1:Int32,V2:Int32) => nop; <_/k>
     if ovf32(V1,V2) ==Int 0

--- misc rules 
rule <k> . => geti(PC) </k>
     <pc>PC:Int32</pc>  

rule  <k> R:Reg => I <_/k>
      <regs_>R |-> I:Int32<_/regs>

rule <k> updateReg(I:Int32, Rd:Reg) => . </k>
     <regs_> Rd |-> (_ => I) <_/regs>


rule <k> incPC(PC) => . <_/k>
     <pc>PC:Int32 => PC +Int32 4</pc>  

rule <k> setPC(1, NewV:Int32) => . <_/k>
     <pc>Pc:Int32 => NewV</pc> 

rule <k> setPC(0, _) => . <_/k>



rule <k> updateDbl(I1:Int32, Rd:Reg, I2:Int32, Rt:Reg) => . <_/k>
     <regs_> Rd |-> (_ => I1) Rt |-> (_ => I2) <_/regs>

rule <k> updateHi(I1:Int32) => . <_/k>
     <hi> _ => I1 </hi>

rule <k> updateLo(I1:Int32) => . <_/k>
     <lo> _ => I1 </lo>
***(
rule  <k> FR:FReg => F <_/k>
      <fregs_>FR |-> F:Float<_/fregs>

rule <k> updateReg(F:Float, FRd:FReg) => . </k>
     <fregs_> FRd |-> (_ => F) <_/fregs>
***)
---
--- one rule per instruction policy
---

rule <k> add Rd:Reg,V1:Int32,V2:Int32; => ovf(V1,V2) ~> 
	updateReg(V1 +Int32 V2, Rd) <_/k>

rule <k> addu Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 +Int32 V2, Rd) </k>

rule <k> mult V1:Int32,V2:Int32; => 
	updateLo((V1 *Int32 V2) %Int32 (1 <<Int32 32)) ~>
        updateHi((V1 *Int32 V2) /Int32 (1 <<Int32 32)) <_/k>

rule <k> div V1:Int32,V2:Int32; => 
	updateLo(V1 /Int32 V2) ~> updateHi(V1 %Int32 V2) <_/k>

rule <k> add Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 +Int32 V2, Rd) </k>

rule <k> addu Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 +Int32 V2, Rd) </k>

rule <k> mult V1:Int32,V2:Int32; => 
	updateLo((V1 *Int32 V2) %Int32 (1 <<Int32 32)) ~>
        updateHi((V1 *Int32 V2) /Int32 (1 <<Int32 32)) <_/k>

rule <k> div V1:Int32,V2:Int32; => 
	updateLo(V1 /Int32 V2) ~> updateHi(V1 %Int32 V2) <_/k>

rule <k> and Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 &Int32 V2, Rd) </k>

rule <k> or Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 |Int32 V2, Rd) </k>

rule <k> j Addr:Int32; => setPC(1, Addr) </k>

rule <k> jr Rs:Reg; => setPC(1, Rs) </k>

rule <k> beq V1:Int32, V2:Int32, Addr; => 
	setPC(Bool2Int(V1 ==Bool V2), Addr) </k>

rule <k> bne V1:Int32, V2:Int32, Addr; => 
	setPC(Bool2Int(V1 =/=Bool V2), Addr) </k>

rule <k> lw Rd:Reg, Off:Int32(V1:Int32); => 
	updateReg(getd(V1 +Int32 Off), Rd) </k>

rule <k> lw Rd:Reg, (V1:Int32 + V2:Int32); => 
	updateReg(getd(V1 +Int32 V2), Rd) </k>

rule <k> sw Rd:Reg, Off:Int32(V1:Int32); => 
	putd(V1 +Int32 Off, Rd) </k>

rule <k> sw Rd:Reg, (V1:Int32 + V2:Int32); => 
	putd(V1 +Int32 V2, Rd) </k>

rule <k> break; => last</k>
     <break> _ => 1</break>
 
--- add rd, rs, rt
rule <k> add Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 +Int32 V2, Rd) </k>

--- addu rd, rs, rt
rule <k> addu Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 +Int32 V2, Rd) </k>

--- addi rd, rs, imm
rule <k> addi Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 +Int32 V2, Rd) </k>

--- addiu rd, rs, imm
rule <k> addiu Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 +Int32 V2, Rd) </k>

--- sub rd, rs, rt
rule <k> sub Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 -Int32 V2, Rd) </k>

--- subu rd, rs, rt
rule <k> subu Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 -Int32 V2, Rd) </k>

--- mult rs, rt
rule <k> mult V1:Int32,V2:Int32; => 
		updateLo((V1 *Int32 V2) %Int32 (1 <<Int32 32)) ~>
                updateHi((V1 *Int32 V2) /Int32 (1 <<Int32 32)) <_/k>

--- multu rs, rt
rule <k> multu V1:Int32,V2:Int32; => 
		updateLo((V1 *Int32 V2) %Int32 (1 <<Int32 32)) ~>
                updateHi((V1 *Int32 V2) /Int32 (1 <<Int32 32)) <_/k>

--- div rs, rt
rule <k> div V1:Int32,V2:Int32; => div0(V2) ~>
	updateLo(V1 /Int32 V2) ~> updateHi(V1 %Int32 V2) <_/k>

--- divu rs, rt
rule <k> divu V1:Int32,V2:Int32; => div0(V2) ~>
	updateLo(V1 /Int32 V2) ~> updateHi(V1 %Int32 V2) <_/k>

--- mfhi rs
rule <k> mfhi Rd; => updateReg(V1 ,Rd) </k>
     <hi>V1:Int32</hi>

--- mthi rs
rule <k> mthi V1; => updateHi(V1) </k>

--- mflo rs
rule <k> mflo Rd; => updateReg(V1 ,Rd) </k>
     <lo>V1:Int32</lo>

--- mtlo rs
rule <k> mtlo V1; => updateLo(V1) </k>

--- and rd, rs, rt
rule <k> and Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 &Int32 V2, Rd) </k>

--- andi rd, rs, imm
rule <k> andi Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 &Int32 V2, Rd) </k>

--- or rd, rs, rt
rule <k> or Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 |Int32 V2, Rd) </k>

--- ori rd, rs, imm
rule <k> ori Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 |Int32 V2, Rd) </k>

--- xor rd, rs, rt
rule <k> xor Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 xorInt32 V2, Rd) </k>

--- xori rd, rs, imm
rule <k> xori Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 xorInt32 V2, Rd) </k>

--- nor rd, rs, rt
rule <k> nor Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(~Int32 (V1 |Int32 V2), Rd) </k>

--- sll rd, rs, amt
rule <k> sll Rd:Reg,V1:Int32,V3:Nat; => 
	updateReg(V1 <<Int32 V3, Rd) </k>

--- sllv rd, rs, rt
rule <k> sllv Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 <<Int32 (V2 &Int32 31), Rd) </k>

--- srl rd, rs, amt
rule <k> srl Rd:Reg,V1:Int32,V3:Nat; => 
	updateReg(V1 >>Int32 V3, Rd) </k>

--- srlv rd, rs, rt
rule <k> srlv Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 >>Int32 (V2 &Int32 31) , Rd) </k>

--- sra rd, rs, amt
rule <k> sra Rd:Reg,V1:Int32,V3:Nat; => 
	updateReg(V1 >>Int32 V3, Rd) </k>

--- srav rd, rs, rt
rule <k> srav Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(V1 >>Int32 V2, Rd) </k>

--- slt rd, rs, rt
rule <k> slt Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(Bool2Int(V1 <Int32 V2), Rd) </k>

--- slti rd, rs, imm
rule <k> slti Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(Bool2Int(V1 <Int32 V2), Rd) </k>

--- sltu rd, rs, rt
rule <k> sltu Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(Bool2Int(V1 <Int32 V2), Rd) </k>

--- sltiu rd, rs, imm
rule <k> sltiu Rd:Reg,V1:Int32,V2:Int32; => 
	updateReg(Bool2Int(V1 <Int32 V2), Rd) </k>

--- j addr
rule <k> j Addr:Int32; => setPC(1, Addr) </k>

--- jr reg
rule <k> jr Rs:Reg; => setPC(1, Rs) </k>

--- jal Imm 
rule <k> jal Imm:Int32; => setPC(1, Imm) </k>

--- jalr reg
rule <k> jalr Rs:Reg; => setPC(1, Rs) </k>

--- beq rs, rt, addr
rule <k> beq V1:Int32, V2:Int32, Addr; => 
	setPC(Bool2Int(V1 ==Bool V2), Addr) </k>

--- bne rs, rt, addr
rule <k> bne V1:Int32, V2:Int32, Addr; => 
	setPC(Bool2Int(V1 =/=Bool V2), Addr) </k>

--- bltz rs, addr
rule <k> bltz V1:Int32, Addr; => 
	setPC(Bool2Int(V1 <Int32 0), Addr) </k>

--- bgtz rs, addr
rule <k> bgtz V1:Int32, Addr; => 
	setPC(Bool2Int(V1 >Int32 0), Addr) </k>

--- blez rs, addr
rule <k> blez V1:Int32, Addr; => 
	setPC(Bool2Int(V1 <=Int32 0), Addr) </k>

--- bgez rs, addr
rule <k> bgez V1:Int32, Addr; => 
	setPC(Bool2Int(V1 >=Int32 0), Addr) </k>

--- lui rt, imm
rule <k> lui Rt:Reg, Imm:Int32; => updateReg(Imm, Rt) </k>

--- lw rd, off(rs)
rule <k> lw Rd:Reg, Off:Int32(V1:Int32); => 
	updateReg(getd(V1 +Int32 Off), Rd) </k>

--- lw rd, (rs+rt)
rule <k> lw Rd:Reg, (V1:Int32 + V2:Int32); => 
	updateReg(getd(V1 +Int32 V2), Rd) </k>

--- lb rd, off(rs)
rule <k> lb Rd:Reg, Off:Int32(V1:Int32); =>
	updateReg(getd(V1 +Int32 Off), Rd) </k>

--- lb rd, (rs+rt)
rule <k> lb Rd:Reg, (V1:Int32 + V2:Int32); => 
	updateReg(getd(V1 +Int32 V2), Rd) </k>

--- lbu rd, off(rs)
rule <k> lbu Rd:Reg, Off:Int32(V1:Int32); => 
	updateReg(getd(V1 +Int32 Off), Rd) </k>

--- lbu rd, (rs+rt)
rule <k> lbu Rd:Reg, (V1:Int32 + V2:Int32); => 
	updateReg(getd(V1 +Int32 V2), Rd) </k>

--- lh rd, off(rs)
rule <k> lh Rd:Reg, Off:Int32(V1:Int32); => 
	updateReg(getd(V1 +Int32 Off), Rd) </k>

--- lh rd, (rs+rt)
rule <k> lh Rd:Reg, (V1:Int32 + V2:Int32); => 
	updateReg(getd(V1 +Int32 V2), Rd) </k>

--- lhu rd, off(rs)
rule <k> lhu Rd:Reg, Off:Int32(V1:Int32); => 
	updateReg(getd(V1 +Int32 Off), Rd) </k>

--- lhu rd, (rs+rt)
rule <k> lhu Rd:Reg, (V1:Int32 + V2:Int32); => 
	updateReg(getd(V1 +Int32 V2), Rd) </k>

--- dlw rd, off(rs)
rule <k> dlw Rd:Reg, Off:Int32(V1:Int32); => 
	updateDbl(getd(V1 +Int32 Off), Rd, 
		  getd((V1 +Int32 Off) +Int32 4), nextReg(Rd)) <_/k>

--- dlw rd, (rs+rt)
rule <k> dlw Rd:Reg, (V1:Int32 + V2:Int32); => 
	updateDbl(getd(V1 +Int32 V2), Rd, 
		  getd((V1 +Int32 V2) +Int32 4), v1) <_/k>

--- sw rd, off(rs)
rule <k> sw Rd:Reg, Off:Int32(V1:Int32); => 
	putd(V1 +Int32 Off, Rd) </k>

--- sw rd, (rs+rt)
rule <k> sw Rd:Reg, (V1:Int32 + V2:Int32); => 
	putd(V1 +Int32 V2, Rd) </k>

--- sb rd, off(rs)
rule <k> sb Rd:Reg, Off:Int32(V1:Int32); => 
	putd(V1 +Int32 Off, Rd) </k>

--- sb rd, (rs+rt)
rule <k> sb Rd:Reg, (V1:Int32 + V2:Int32); => 
	putd(V1 +Int32 V2, Rd) </k>
     
--- sh rd, off(rs)
rule <k> sh Rd:Reg, Off:Int32(V1:Int32); => 
	putd(V1 +Int32 Off, Rd) </k>

--- sh rd, (rs+rt)
rule <k> sh Rd:Reg, (V1:Int32 + V2:Int32); => 
	putd(V1 +Int32 V2, Rd) </k>

--- dsw rd, off(rs)
rule <k> dsw Rd:Reg, Off:Int32(V1:Int32); => 
	putdDbl((V1 +Int32 Off), Rd, 
	        (V1 +Int32 Off) +Int32 4, nextReg(Rd)) <_/k>

--- dsw rd, (rs+rt)
rule <k> dsw Rd:Reg, (V1:Int32 + V2:Int32); => 
	putdDbl((V1 +Int32 V2), Rd, 
	        (V1 +Int32 V2) +Int32 4, nextReg(Rd)) <_/k>

--- dsz rd, off(rs)
rule <k> dsz Rd:Reg, Off:Int32(V1:Int32); => 
	putd(V1 +Int32 Off, Rd) </k>

--- dsz rd, (rs+rt)
rule <k> dsz Rd:Reg, (V1:Int32 + V2:Int32); => 
	putd(V1 +Int32 V2, Rd) </k>

--- nop
rule <k> nop; => incPC(PC) <_/k>
     <pc>PC:Int32</pc>

--- break
rule <k> break; => last </k>
     <break> _ => 1</break>

--- pseudo-instruction for div
rule <k> div Rd:Reg, V1:Int32, V2:Int32; => nop; <_/k>



***(
--- add.s rd, rs, rt
rule <k> add.s FRd:FReg,FV1:Float,FV2:Float; => 
	updateReg(FV1 +Float FV2, FRd) </k>

--- add.d rd, rs, rt
rule <k> add.d FRd:FReg,FV1:Float,FV2:Float; => 
	updateReg(FV1 +Float FV2, FRd) </k>

--- sub.s rd, rs, rt
rule <k> sub.s FRd:FReg,FV1:Float,FV2:Float; => 
	updateReg(FV1 -Float FV2, FRd) </k>

--- sub.d rd, rs, rt
rule <k> sub.d FRd:FReg,FV1:Float,FV2:Float; => 
	updateReg(FV1 -Float FV2, FRd) </k>

--- mul.s rd, rs, rt
rule <k> mul.s FRd:FReg,FV1:Float,FV2:Float; => 
	updateReg(FV1 *Float FV2, FRd) </k>

--- mul.d rd, rs, rt
rule <k> mul.d FRd:FReg,FV1:Float,FV2:Float; => 
	updateReg(FV1 *Float FV2, FRd) </k>

--- div.s rd, rs, rt
rule <k> div.s FRd:FReg,FV1:Float,FV2:Float; => 
	updateReg(FV1 /Float FV2, FRd) </k>

--- div.d rd, rs, rt
rule <k> div.d FRd:FReg,FV1:Float,FV2:Float; => 
	updateReg(FV1 /Float FV2, FRd) </k>


***)
endkm

