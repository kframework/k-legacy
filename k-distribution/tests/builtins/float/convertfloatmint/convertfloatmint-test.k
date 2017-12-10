requires "domains.k"

module CONVERTFLOATMINT-TEST-SYNTAX
    imports MINT
    imports FLOAT

    syntax Task ::=  "test" "(" Int "," Float  "," Int "," Int "," Int ")" [function]
    syntax Tasks ::= List{Task, ""} 
endmodule

module CONVERTFLOATMINT-TEST
    imports CONVERTFLOATMINT-TEST-SYNTAX

    configuration <k>$PGM:Tasks</k>

    rule T:Task Ts:Tasks => T ~> Ts
    rule test(I, F,  W, P, E) =>  eqMInt(mi(W, I), Float2MInt(MInt2Float(mi(W,I), P,E), W)) 
                                andBool F ==Float MInt2Float(Float2MInt(F,W), P, E) 
                                andBool MInt2Float(mi(W,I), P,E) ==Float F 
                                andBool eqMInt(Float2MInt(F,W), mi(W,I))
endmodule
