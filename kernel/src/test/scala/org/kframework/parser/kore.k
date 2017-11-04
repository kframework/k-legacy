// Copyright (c) 2015-2016 K Team. All Rights Reserved.


module KBUBBLE
  // TODO: Must make the parser actually accept reject2 in this format (as opposed to vertical bars)
  syntax KBubbleItem ::= r"[^ \\t\\n\\r]+" [token, reject2(rule|syntax|endmodule|configuration|context)]
  syntax KBubble ::= KBubble KBubbleItem   [token]
  syntax KBubble ::= KBubbleItem           [token]
endmodule


module KTOKENS
  syntax KSymbol ::= r"[\\.A-Za-z\\-0-9]*"                    [token]
  syntax KSymbol ::= r"`(\\\\`|\\\\\\\\|[^`\\\\\n\r\t\f])+`"  [token]
  syntax KSymbol ::= r"(?![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*" [token, autoReject]

  // syntax KSymbol ::= """(?<![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*"""
  // TODO: the (?<! is a signal to the parser that it should be used as a "precedes" clause, do we need it?

  syntax KString ::= r"[\"](([^\n\r\t\f\"\\\\])|([\\\\][nrtf\"\\\\])|([\\\\][x][0-9a-fA-F]{2})|([\\\\][u][0-9a-fA-F]{4})|([\\\\][U][0-9a-fA-F]{8}))*[\"]" [token]
endmodule


module KML
  imports KTOKENS

  syntax KMLVariable ::= KSymbol ":" KSymbol [klabel(KMLVariable)]
  syntax KMLPattern ::= KMLVariable
  syntax KMLPattern ::= KSymbol

  // every <SORT> should have a production like this for variables
  // syntax <SORT> ::= KID ":" "<SORT>"

  syntax KMLPattern ::= "tt" [klabel(KMLTrue)]
  syntax KMLPattern ::= "ff" [klabel(KMLFalse)]

  syntax KMLPattern ::= "~" KMLPattern              [klabel(KMLNot)]
  syntax KMLPattern ::= KMLPattern "/\\" KMLPattern [klabel(KMLAnd)]
  syntax KMLPattern ::= KMLPattern "\\/" KMLPattern [klabel(KMLOr)]
  syntax KMLPattern ::= KMLPattern "->"  KMLPattern [klabel(KMLImplies)]

  syntax KMLPattern ::= "E" KMLVariable "." KMLPattern [klabel(KMLExists)]
  syntax KMLPattern ::= "A" KMLVariable "." KMLPattern [klabel(KMLForAll)]

  syntax KMLPattern ::= KMLPattern "==" KMLPattern [klabel(KMLEquals)]
  syntax KMLPattern ::= "next" KMLPattern          [klabel(KMLNext)]
  syntax KMLPattern ::= KMLPattern "=>" KMLPattern [klabel(KMLRewrite)]

  syntax KMLPatternList ::= "" [klabel(.KMLPatternList)]
  syntax KMLPatternList ::= KMLPattern
  syntax KMLPatternList ::= KMLPattern "," KMLPatternList [klabel(KMLPatternList)]

  syntax KMLPattern ::= KSymbol "(" KMLPatternList ")"    [klabel(KMLApplication)]
  syntax KMLPattern ::= "val" "(" KSymbol "," KString ")" [klabel(KMLDomainValue)]
endmodule


module KSENTENCE
  imports KML
  imports KBUBBLE

  syntax KTerminal ::= KString
  syntax KTerminal ::= "r" KString [klabel(KRegex)]
  syntax KNonTerminal ::= KSymbol

  syntax KProduction ::= KTerminal
  syntax KProduction ::= KNonTerminal
  syntax KProduction ::= KProduction KProduction [klabel(KProduction), assoc]

  syntax KPriority ::= KMLPatternList
  syntax KPriority ::= KPriority ">" KPriority [klabel(KPriorityItems), assoc]

  syntax KAttributes ::= "" [klabel(.KAttributes)]
  syntax KAttributes ::= "[" KMLPatternList "]" [klabel(KAttributes)]

  syntax KSentence ::= "imports" KSymbol KAttributes                  [klabel(KImport)]
  syntax KSentence ::= "syntax" KSymbol KAttributes                   [klabel(KSortDeclaration)]
  syntax KSentence ::= "syntax" KSymbol "::=" KProduction KAttributes [klabel(KSyntaxProduction)]
  syntax KSentence ::= "syntax" KSymbol "::=" KMLPattern KAttributes  [klabel(KSymbolDeclaration)]
  syntax KSentence ::= "syntax" "priority" KPriority KAttributes      [klabel(KSyntaxPriority)]
  syntax KSentence ::= "rule" KBubble KAttributes                     [klabel(KRule)]

  syntax KSentenceList ::= KSentence
  syntax KSentenceList ::= "" [klabel(.KSentenceList)]
  syntax KSentenceList ::= KSentence KSentenceList [klabel(KSentenceList)]

  // TODO: Why doesn't this work? Answer: KSentenceList can be empty syntax, parsing ambiguities
  //syntax KSentenceList ::= KSentenceList KSentenceList [klabel(KSentenceList), assoc, comm, unit(.KSentenceList)]
endmodule


module KDEFINITION
  imports KSENTENCE

  // syntax KRequire ::= "require" KString [klabel(KRequire)]
  // syntax KRequireList ::= "" [klabel(.KRequireList)]
  // syntax KRequireList ::= KRequire KRequireList [klabel(KRequireList)]

  syntax KModule ::= "module" KSymbol KSentenceList "endmodule" KAttributes [klabel(KModule)]
  syntax KModuleList ::= "" [klabel(.KModuleList)]
  syntax KModuleList ::= KModule KModuleList [klabel(KModuleList)]

  syntax KDefinition ::= KAttributes KModuleList [klabel(KDefinition)]
endmodule [mainModule]
