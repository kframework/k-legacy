module LANG
  syntax Foo ::= "foo"

  configuration <k> $PGM:K </k>
                <state> .Map </state>
                <storedState> .K </storedState>
                <exit exit=""> 0 </exit>

  syntax KItem ::= savedState(StateCell)
  syntax Bool ::= bar(StateCell) [function]

  rule <k> foo => .K ...</k>
       <state> M::Map </state>
       <storedState> _ => savedState(<state> M </state>) </storedState>
    requires bar(<state> M </state>)
  rule bar(<state> .Map </state>) => true
endmodule
