// Copyright (c) 2015-2016 K Team. All Rights Reserved.

[ #MainModule(KDEFINITION)
, #EntryModules(KDEFINITION)
]


module KBUBBLE
  syntax KBubbleItem ::= r"[^ \n\r\t]+" [token, reject2(rule|syntax|endmodule|configuration|context)]
  syntax KBubble ::= KBubble KBubbleItem   [token]
                   | KBubbleItem           [token]
endmodule


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
  syntax KMLPattern ::= r"[A-Za-z0-9\\.@#\\|\\-]+" [token]

  syntax KMLPattern ::= "tt" [klabel(KMLTrue)]
  syntax KMLPattern ::= "ff" [klabel(KMLFalse)]

  syntax KMLPattern ::= KMLPattern "/\\" KMLPattern [klabel(KMLAnd)]
  syntax KMLPattern ::= KMLPattern "\\/" KMLPattern [klabel(KMLOr)]
  syntax KMLPattern ::= "~" KMLPattern              [klabel(KMLNot)]

  syntax KMLPattern ::= KMLPattern "->"  KMLPattern    [klabel(KMLImplies)]
  syntax KMLPattern ::= "E" KMLVariable "." KMLPattern [klabel(KMLExists)]
  syntax KMLPattern ::= "A" KMLVariable "." KMLPattern [klabel(KMLForAll)]

  syntax KMLPattern ::= "next" KMLPattern          [klabel(KMLNext)]
  syntax KMLPattern ::= KMLPattern "=>" KMLPattern [klabel(KMLRewrite)]
  syntax KMLPattern ::= KMLPattern "==" KMLPattern [klabel(KMLEquals)]

  syntax KMLPatternList ::= "" [klabel(.KMLPatternList)]
  syntax KMLPatternList ::= KMLPattern
  syntax KMLPatternList ::= KMLPattern "," KMLPatternList [klabel(KMLPatternList)]

  syntax KMLPattern ::= KSymbol "(" KMLPatternList ")"    [klabel(KMLApplication)]
endmodule


module KSENTENCE
  imports KML
  imports KBUBBLE

  syntax KAttributes ::= ""                     [klabel(.KAttributes)]
  syntax KAttributes ::= "[" KMLPatternList "]" [klabel(KAttributes)]

  syntax KSentence ::= "imports" KSymbol KAttributes                                 [klabel(KImport)]
  syntax KSentence ::= "syntax" KSymbol ":=" KSymbol "(" KSymbolList ")" KAttributes [klabel(KSymbolDeclaration)]
  syntax KSentence ::= "rule" KBubble KAttributes                                    [klabel(KRule)]

  syntax KSentenceList ::= KSentence
  syntax KSentenceList ::= "" [klabel(.KSentenceList)]
  syntax KSentenceList ::= KSentence KSentenceList [klabel(KSentenceList)]
endmodule


module KDEFINITION
  imports KSENTENCE

  syntax KModule ::= "module" KSymbol KSentenceList "endmodule" KAttributes [klabel(KModule)]
  syntax KModuleList ::= ""                                                 [klabel(.KModuleList)]
  syntax KModuleList ::= KModule KModuleList                                [klabel(KModuleList)]

  syntax KDefinition ::= KAttributes KModuleList [klabel(KDefinition)]
endmodule
