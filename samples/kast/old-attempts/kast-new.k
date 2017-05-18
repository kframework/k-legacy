// Copyright (c) 2014 K Team. All Rights Reserved.

module K-SORT
  syntax K
endmodule


module OUTER
  import K-SORT

  // Top level module stuff
  syntax KDefinition   ::= KRequireList KModuleList
  syntax KRequireList  ::= List{KRequire,""}
  syntax KRequire      ::= "require" KString
  syntax KModuleList   ::= List{KModule,""}
  syntax KModule       ::= "module" KModuleName KImportList KSentenceList "endmodule"
  syntax KImportList   ::= List{KImport,""}
  syntax KImport       ::= "import" KModuleName
  syntax KSentenceList ::= List{KSentence,""}

  syntax KModuleName   ::= Token{"[A-Z\-]+"}

  syntax KSentence ::= KSyntacticSentence
                     | KSemanticSentence

  syntax KSyntacticSentence ::= "syntax" KSort "::=" KPriorityBlockList
                              | "syntax" KSort
                              | "syntax" KSort KAttributes
  syntax KPriorityBlockList ::= NeList{KPriorityBlock,">"}
  syntax KPriorityBlock ::=             KProductionList
                         | "left:"      KProductionList
                         | "right:"     KProductionList
                         | "non-assoc:" KProductionList
  syntax KProductionList  ::= NeList{KProductionEntry,"|"}
  syntax KProductionEntry ::= KProduction KAttributes
  syntax KProduction ::= List{ProductionItem,""}
                       | KLabel "(" KSortList ")"
  syntax KProductionItem ::= KSort                            // non-terminal
                           | KString                          // terminals
                           | "Token{" KRegEx "}"              // token
                           | "List{" KSort "," KString "}"    // lists
                           | "NeList{" KSort "," KString "}"  // nelists
                           // maybe more

  syntax KSemanticSentence ::= "configuration" K KAttributes
                             | "rule"          K KAttributes
                             | "context"       K KAttributes

  syntax KAttributes ::= "" [onlyLabel, klabel(noAttributes)]
                       | "[" KAttributeList "]"
                       
  syntax KAttributeList ::= List{KAttribute,","}

  syntax KAttribute ::= KTag
                      | KTag "(" KTagList ")"

  syntax KString ::= ...
  syntax KSort ::= ...
  syntax KTag ::= ...
  syntax KTagList ::= List{KTag,","}

endmodule


module BUBBLE is
  syntax Bubble ::= List{BubbleItem,""}   [token]
  syntax BubbleItem ::= Token{"dummy"} [regex([^`\s]\S*)]
                      | Token{"dummy"} [regex([\`]{1}([^\`])+[\`]{1})]
                      | Token{"dummy"} [regex([\`]{2}([^\`]|[\`][^\`])+[\`]{2})]
                      | Token{"dummy"} [regex([\`]{3}([^\`]|[\`][^\`]|[\`][\`][^\`])+[\`]{3})]
                      // can add more if we want to
					  | "true"
					  | "false"
endmodule


module OUTER-WITH-BUBBLES
  import OUTER
  import BUBBLE
  syntax K ::= Bubble
endmodule


module KAST
  import K-SORT
  syntax KLabel    ::= Token{...}
                     | "`" KLabel "`"  [bracket]
  syntax KConstant ::= #token(KString, KString)
                     | #klabel(KLabel)
  syntax KItem     ::= KConstant
                     | KLabel "(" KList ")"
  syntax KList     ::= NeList{K,","}
                     | ".::KList"
  // we need to say something like List{K}{","}{"."}
  syntax K         ::= NeList{KItem,"~>"}
                     | ".::K"
  // we need to say List{KItem}{"~>"}{"."}

  syntax KVariable ::= Token{"dummy"} [notInRules, regex(\$?[A-Z_][A-Za-z0-9]*)]
  syntax KItem  ::= KVariable
                  | KItem ":" KSort  // KItem must obey its sort at runtime
                                     // sorting arbitrary KItems; Sort can also be
                                     // K, KItem, KToken, but not KList
  syntax K ::= K "=>" K
             | K "requires" K
             | K "ensures" K
             | "`" K "`"  [bracket]
endmodule


module OUTER-WITH-KAST
  import OUTER
  import KAST
  rule #token(S:KString, "KAny") => #parse(S, "KDefinition", "OUTER-WITH-BUBBLES")
  rule #token(S:KString, "Bubble") => #parse(S, "K", "KAST")
endmodule
