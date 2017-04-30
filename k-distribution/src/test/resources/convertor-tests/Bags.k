module TEST-SYNTAX
 syntax Stmt  ::= "COUNTER" "=" Id ";"
endmodule
module TEST
 imports TEST-SYNTAX
 syntax KResult ::= Int | Bool | String
 configuration <T color="yellow">
                   <k> $PGM:Stmt</k>
                 <counters>
                 <counter multiplicity="*"> .K </counter>
                 </counters>
               </T>


   rule <k> COUNTER=I; =>. ...</k>
        <counters> CT:Bag(.Bag =><counter> I </counter>) </counters>
endmodule
