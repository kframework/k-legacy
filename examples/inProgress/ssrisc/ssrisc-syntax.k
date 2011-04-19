kmod SSRISC-SYNTAX is including PL-INT32 + PL-UNSIGNED-INT32 + PL-ID + PL-FLOAT + K 

syntax Reg ::= Int32 

syntax FReg ::= Float

syntax Imm ::= Int32

syntax Amt ::= Nat

syntax Addr ::= Int32

syntax Off ::= Int32

syntax Instr ::= add Reg , Reg , Reg ; [strict (2 3)]
               | addu Reg , Reg , Reg ; [strict (2 3)]
	       | addi Reg , Reg , Imm ; [strict (2)]
               | addiu Reg , Reg , Imm ; [strict (2)]
               | sub Reg , Reg , Reg ; [strict (2 3)]
               | subu Reg , Reg , Reg ; [strict (2 3)]
               | mult Reg , Reg ; [strict (1 2)]               
               | multu Reg , Reg ; [strict (1 2)]
               | div Reg , Reg ; [strict (1 2)]               
               | divu Reg , Reg ; [strict (1 2)]
               | mthi Reg ; [strict]               
               | mfhi Reg ; [strict]
               | mtlo Reg ; [strict]               
               | mflo Reg ; [strict]
	       | and Reg , Reg , Reg ; [strict (2 3)]
	       | andi Reg , Reg , Imm ; [strict (2)]
               | or Reg , Reg , Reg ; [strict (2 3)]
               | ori Reg , Reg , Imm ; [strict (2)]
               | xor Reg , Reg , Reg ; [strict (2 3)]
	       | xori Reg , Reg , Imm ; [strict (2)] 
               | nor Reg , Reg , Reg ; [strict (2 3)]
	       | sll Reg , Reg , Amt ; [strict (2)]
	       | sllv Reg , Reg , Reg ; [strict (2 3)]
               | srl Reg , Reg , Amt ; [strict (2)]
	       | srlv Reg , Reg , Reg ; [strict (2 3)]
               | sra Reg , Reg , Amt ; [strict (2)]
	       | srav Reg , Reg , Reg ; [strict (2 3)]
               | slt Reg , Reg , Reg ; [strict (2 3)]
               | slti Reg , Reg , Imm ; [strict (2)]
               | sltu Reg , Reg , Reg ; [strict (2 3)]
               | sltiu Reg , Reg , Imm ; [strict (2)]
               | j Addr ; [strict]
               | jr Reg ; [strict]
               | jal Addr ; [strict]
               | jalr Reg ; [strict]
               | bne Reg , Reg , Addr ; [strict (1 2)]
               | beq Reg , Reg , Addr ; [strict (1 2)]
               | bltz Reg , Addr ; [strict (1)]
               | bgtz Reg , Addr ; [strict (1)]
               | blez Reg , Addr ; [strict (1)]
               | bgez Reg , Addr ; [strict (1)]
               | lw Reg , Off ( Reg ) ; [strict (3)]
               | lw Reg , ( Reg + Reg ) ; [strict (2 3)]
               | lb Reg , Off ( Reg ) ; [strict (3)]
               | lb Reg , ( Reg  + Reg ) ; [strict (2 3)]
               | lbu Reg , Off ( Reg ) ; [strict (3)]
               | lbu Reg , ( Reg  + Reg ) ; [strict (2 3)]
               | lh Reg , Off ( Reg ) ; [strict (3)]
               | lh Reg , ( Reg  + Reg ) ; [strict (2 3)]
               | lhu Reg , Off ( Reg ) ; [strict (3)]
               | lhu Reg , ( Reg  + Reg ) ; [strict (2 3)]
               | dlw Reg , Off ( Reg ) ; [strict (3)]
               | dlw Reg , ( Reg  + Reg ) ; [strict (2 3)]
               | lui Reg , Imm ;
               | sw Reg , Off ( Reg ) ; [strict (3)]
               | sw Reg , ( Reg + Reg ) ; [strict (2 3)]
               | sb Reg , Off ( Reg ) ; [strict (3)]
               | sb Reg , ( Reg  + Reg ) ; [strict (2 3)]
               | sh Reg , Off ( Reg ) ; [strict (3)]
               | sh Reg , ( Reg  + Reg ) ; [strict (2 3)]
               | dsw Reg , Off ( Reg ) ; [strict (3)]
               | dsw Reg , ( Reg  + Reg ) ; [strict (2 3)]
               | dsz Reg , Off ( Reg ) ; [strict (3)]
               | dsz Reg , ( Reg  + Reg ) ; [strict (2 3)]
               | nop ;
	       | break ;
--- pseudo-instructions
               | div Reg , Reg , Reg ; [strict (2 3)]
 
               --- some floating point instructions
               | add.s FReg , FReg , FReg ; [strict (2 3)]
               | add.d FReg , FReg , FReg ; [strict (2 3)]
               | sub.s FReg , FReg , FReg ; [strict (2 3)]
               | sub.d FReg , FReg , FReg ; [strict (2 3)]
               | mul.s FReg , FReg , FReg ; [strict (2 3)]
               | mul.d FReg , FReg , FReg ; [strict (2 3)]
               | div.s FReg , FReg , FReg ; [strict (2 3)]
               | div.d FReg , FReg , FReg ; [strict (2 3)]


***(
	       | abs.s FReg , FReg ; [strict (2)]
	       | abs.d FReg , FReg ; [strict (2)]
	       | mov.s FReg , FReg ; [strict (2)]
	       | mov.d FReg , FReg ; [strict (2)]
	       | neg.s FReg , FReg ; [strict (2)]
	       | neg.d FReg , FReg ; [strict (2)]
	       | cvt.s.d FReg , FReg ; [strict (2)]
	       | cvt.s.w FReg , FReg ; [strict (2)]
	       | cvt.d.s FReg , FReg ; [strict (2)]
	       | cvt.d.w FReg , FReg ; [strict (2)]
	       | cvt.w.s FReg , FReg ; [strict (2)]
	       | cvt.w.d FReg , FReg ; [strict (2)]
--- bug	       | c.eq.s FReg , FReg ; [strict]
--- bug	       | c.eq.d FReg , FReg ; [strict]
	       | c.lt.s FReg , FReg ; [strict]
	       | c.lt.d FReg , FReg ; [strict]
	       | c.le.s FReg , FReg ; [strict]
	       | c.le.d FReg , FReg ; [strict]
	       | sqrt.s FReg , FReg ; [strict (2)]
	       | sqrt.d FReg , FReg ; [strict (2)]
               | lui Reg , Imm ; 
               | mfc1 Reg , FReg ; [strict (2)]
	       | mtc1 Reg , FReg ; [strict (2)]
***)

syntax Helpers ::= updateReg ( K , K ) [strict(1)]
		 | updateDbl ( K , K , K , K ) [strict(1 3)]
                 | updateHi ( K ) [strict]
                 | updateLo ( K ) [strict]
                 | ovf ( K , K ) [strict]
		 | incPC ( K ) [strict]
		 | setPC ( K , K ) [strict(2)]
                 | updMem ( K ) [strict]
                 
--- I may need to generalize to all registers 
syntax Reg ::= v0 | v1
syntax Reg ::= nextReg ( Reg ) 

syntax K ::= Instr | Imm | Amt | Addr | Off | Helpers

syntax Prog ::= Map

endkm

