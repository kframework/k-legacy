// Copyright (c) 2015-2016 K Team. All Rights Reserved.

[ #MainModule(KDEFINITION)
, #EntryModules(KDEFINITION)
]


module KTOKENS
  syntax KSymbol ::= r"[A-Za-z0-9\\.@#\\|\\-]+"  [token]
                   | r"`[^\n\r\t\f]+`"           [token]
  syntax KSymbolList ::= ""                      [klabel(.KSymbolList)]
                       | KSymbol
                       | KSymbol "," KSymbolList [klabel(KSymbolList)]

  syntax KString ::= r"[\"](([^\n\r\t\f\"\\\\])|([\\\\][nrtf\"\\\\])|([\\\\][x][0-9a-fA-F]{2})|([\\\\][u][0-9a-fA-F]{4})|([\\\\][U][0-9a-fA-F]{8}))*[\"]" [token]
endmodule


module KML
  imports KTOKENS

  syntax KMLVariable ::= KSymbol ":" KSymbol [klabel(KMLVariable)]

  syntax KMLPattern ::= KMLVariable
                      | r"[A-Za-z0-9\\.@#\\|\\-]+" [token]

                      | "tt" [klabel(KMLTrue)]
                      | "ff" [klabel(KMLFalse)]

                      | KMLPattern "/\\" KMLPattern [klabel(KMLAnd)]
                      | KMLPattern "\\/" KMLPattern [klabel(KMLOr)]
                      | "~" KMLPattern              [klabel(KMLNot)]

                      | KMLPattern "->"  KMLPattern    [klabel(KMLImplies)]
                      | "E" KMLVariable "." KMLPattern [klabel(KMLExists)]
                      | "A" KMLVariable "." KMLPattern [klabel(KMLForAll)]

                      | "next" KMLPattern          [klabel(KMLNext)]
                      | KMLPattern "=>" KMLPattern [klabel(KMLRewrite)]
                      | KMLPattern "==" KMLPattern [klabel(KMLEquals)]

                      | KSymbol "(" KMLPatternList ")"    [klabel(KMLApplication)]

  syntax KMLPatternList ::= "" [klabel(.KMLPatternList)]
                          | KMLPattern
                          | KMLPattern "," KMLPatternList [klabel(KMLPatternList)]
endmodule


module KSENTENCE
  imports KML

  syntax KAttributes ::= ""                     [klabel(.KAttributes)]
                       | "[" KMLPatternList "]" [klabel(KAttributes)]

  syntax KSentence ::= "imports" KSymbol KAttributes                                 [klabel(KImport)]
                     | "syntax" KSymbol ":=" KSymbol "(" KSymbolList ")" KAttributes [klabel(KSymbolDeclaration)]
                     | "rule" KMLPattern KAttributes                                 [klabel(KRule)]

  syntax KSentenceList ::= KSentence
                         | "" [klabel(.KSentenceList)]
                         | KSentence KSentenceList [klabel(KSentenceList)]
endmodule


module KDEFINITION
  imports KSENTENCE

  syntax KModule ::= "module" KSymbol KSentenceList "endmodule" KAttributes [klabel(KModule)]

  syntax KModuleList ::= ""                                                 [klabel(.KModuleList)]
                       | KModule KModuleList                                [klabel(KModuleList)]

  syntax KDefinition ::= KAttributes KModuleList [klabel(KDefinition)]
endmodule
