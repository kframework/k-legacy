module A-SYNTAX

syntax Exp ::= "symInt"
             | "symBool"
             | "symString"
             | Int
             | Bool
             | String

syntax Exp ::= if(Exp) [strict]

endmodule

module A

imports A-SYNTAX

syntax KResult ::= Int
                 | Bool
                 | String

rule symInt => ?_:Int
rule symBool => ?_:Bool
rule symString => ?_:String

rule if(0) => 0 [transition]
rule if(1) => 1 [transition]

rule if(true ) => true [transition]
rule if(false) => false [transition]

rule if("t") => "t" [transition]
rule if("f") => "f" [transition]
rule if("") => "default" [transition]

endmodule
