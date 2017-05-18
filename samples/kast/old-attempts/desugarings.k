The desugaring ruls below are quite informal at this stage.

*) List and NeList productions desugar to normal productions, with
attributes like "assoc".

rule `syntax L:KSort ::= List{S:KSort, T:KString}`
  => `syntax L ::= "_" + T + "_" + "(" + S + "," + S + ")" + "[" assoc "]"`

*) 