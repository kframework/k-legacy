// Copyright (c) 2015-2016 K Team. All Rights Reserved.
requires "kast.k"

module BUBBLE

  syntax Bubble ::= Bubble BubbleItem [token]
                  | BubbleItem        [token]
  syntax BubbleItem ::= r"[^ \t\n\r]" [token, reject2("rule|syntax|endmodule|configuration|context")]

endmodule

module ATTRIBUTES
  imports KSTRING
  imports BASIC-K
  syntax KEY        ::= r"[a-z][A-Za-z\\-0-9]*" [token]
  syntax TAGList    ::= TAGCONTENT              [token]
  syntax TAGCONTENT ::= TAGCONTENT TC           [klabel(#TAGCONTENTList)] // dummy klabel
                      | TC                      [klabel(#TAGCONTENTTC)]   // dummy klabel
                      | ""                      [klabel(#NOTAGCONTENT)]   // dummy klabel
  syntax TC ::= r"[^\\n\\r\\(\\)\\\"]+"         [klabel('TC)]             // dummy klabel
              | "(" TAGCONTENT ")"

  syntax Attr ::= KEY                 [klabel(#TagSimple), latex(\kattribute{#1})]
                | KEY "(" TAGList ")" [klabel(#TagContent), latex(\kattribute{#1}(#2))]
                | KEY "(" KString ")" [klabel(#TagString), latex(\kattribute{#1}(#2))]
endmodule

// To be used for first-level parsing/pretty-printing of global KORE
// definitions, where the K terms are put in bubbles.  A similar, but
// larger OUTER module can be defined for arbitrary K definitions.
module OUTER-KORE
  imports KSTRING
  imports BASIC-K
  imports BUBBLE
  imports ATTRIBUTES

  syntax KDefinition   ::= KRequireList KModuleList
                               [klabel(#KDefinition), hook(org.kframework.definition.Definition), latex(\begin{document}\n#1\n#2\n\end{document})]

  syntax KRequire      ::= "require" KString
                               [klabel(#KRequire), hook(org.kframework.definition.Require)]

  syntax KRequireList  ::= ""  [klabel(#emptyKRequireList), onlyLabel]
                         | KRequireList KRequire
                               [klabel(#KRequireList), assoc, comm, unit(#emptyKRequireList), latex(#1\n#2)]

  syntax KModule       ::= "module" KModuleName OptionalAttributes
                                    KImportList
                                    KSentenceList
                           "endmodule"
                               [klabel(#KModule), hook(org.kframework.definition.Module), latex(\begin{module}{\n\moduleName{#1}#2\n#3#4\n\end{module})]
  syntax KModuleList   ::= ""  [klabel(#emptyKModuleList), onlyLabel]
                         | KModuleList KModule
                               [klabel(#KModuleList), assoc, comm, unit(#emptyKModuleList)]

  syntax KImport       ::= "imports" KModuleName
                               [klabel(#KImport), hook(org.kframework.definition.Import), latex(  import #1\n)]

  syntax KImportList   ::= ""  [klabel(#emptyKImportList), onlyLabel]
                         | KImportList KImport
                               [klabel(#KImportList), assoc, comm, unit(#emptyKImportList), latex(#1#2)]

  syntax KSentenceList ::= ""  [klabel(#emptyKSentenceList), onlyLabel]
                         | KSentenceList KSentence
                               [klabel(#KSentenceList), assoc, comm, unit(#kemptyKSentenceList), latex(#1\n#2)]

  syntax KSentence ::= "syntax" KSort OptionalAttributes
                               [klabel(#KSyntaxSort), hook(org.kframework.definition.SyntaxSort), latex(  \begin{syntaxBlock}\n    #1#2\n  \end{syntaxBlock})]
                     | "syntax" KSort "::=" PrioritySeqBlock
                               [klabel(#KSyntaxProduction), hook(org.kframework.definition.SyntaxProduction), latex(  \begin{syntaxBlock}\n    #1 ::= #2\n  \end{syntaxBlock})]
                     | "syntax" "priority"   KPrioritySeq OptionalAttributes
                               [klabel(#KSyntaxPriority), hook(org.kframework.definition.SyntaxPriority), latex(  \begin{syntaxBlock}\n    priority #1#2\n  \end{syntaxBlock})]
                     | "syntax" "priorities" KPrioritySeq OptionalAttributes
                               [klabel(#KSyntaxPriorities), hook(org.kframework.definition.SyntaxPriority), latex(  \begin{syntaxBlock}\n    priorities #1#2\n  \end{syntaxBlock})]
                     | "syntax" "left" KNeTagSet OptionalAttributes
                               [klabel(#KSyntaxLeft), hook(org.kframework.definition.SyntaxPriority), latex(  \begin{syntaxBlock}\n    left #1#2\n  \end{syntaxBlock})]
                     | "syntax" "right" KNeTagSet OptionalAttributes
                               [klabel(#KSyntaxRight), hook(org.kframework.definition.SyntaxPriority), latex(  \begin{syntaxBlock}\n    right #1#2\n  \end{syntaxBlock})]
                     | "syntax" "non-assoc" KNeTagSet OptionalAttributes
                               [klabel(#KSyntaxNonAssoc), hook(org.kframework.definition.SyntaxPriority), latex(  \begin{syntaxBlock}\n    non-assoc #1#2\n  \end{syntaxBlock})]

  syntax KPrioritySeq ::= KPrioritySeq ">" KNeTagSet   [klabel(#KPrioritySeq), assoc]
                        | KNeTagSet
  syntax KNeTagSet    ::= Tag KNeTagSet                [klabel(#KTagSet), assoc, comm]
                        | Tag
  syntax Tag ::= KLabel

  syntax KProduction ::= KProductionItem
                       | KProduction KProductionItem
                               [klabel(#KProduction), assoc, unit(#emptyKProduction)]
  syntax KProductionItem ::= KSort       [hook(org.kframework.definition.NonTerminal)]
                           | KString     [hook(org.kframework.definition.Terminal)]
                           | "r" KString [hook(org.kframework.definition.RegexTerminal)]
                           | "Token" "{" TokenContent "}" [klabel(#KPIToken)]
                           | "Lexer" "{" TokenContent "}" [klabel(#KPILexer)]
                           | "NeList" "{" KSort "," KString "}" [klabel(#NeList)]
                           |   "List" "{" KSort "," KString "}" [klabel(#List)]
  syntax TokenContent ::= r"[^\\n\\r}]" [token]
  syntax PrioritySeqBlock ::= PrioritySeqBlock ">" AssocAttribute ProdBlock [klabel(#PrioritySeqBlock)]
                            | ProdBlock
  syntax AssocAttribute ::= ""           [klabel(#NoAttribute)]
                          | "left:"      [klabel(#LeftAttribute)]
                          | "right:"     [klabel(#RightAttribute)]
                          | "non-assoc:" [klabel(#NonAssocAttribute)]
  syntax ProdBlock ::= ProdBlock "|" KProductionWAttr [klabel(#ProdBlock)]
                     | KProductionWAttr
  syntax KProductionWAttr ::= KProduction OptionalAttributes [klabel(#KProductionWAttr)]
                            | KLabel "(" KSortList ")" OptionalAttributes [klabel(#KFuncProductionWAttr)]
                            |        "(" KSortList ")" OptionalAttributes [klabel(#KTupleProductionWAttr)]
  syntax KSortList ::= KSortList "," KSort [klabel(#KSortList)]
                     | KSort
  // We use #KAttributes as top KLabel in the K term holding the attributes
  syntax OptionalAttributes ::= KAttributesDeclaration
                              | "" [klabel(#NoKAttributesDeclaration)]
  syntax KAttributesDeclaration ::= "[" AttrList "]" [klabel(#KAttributesDeclaration), hook(org.kframework.kore.KAttributes)]
  syntax AttrList ::= AttrList "," Attr [klabel(#KAttributesList)]
                    | Attr

  syntax KSentence ::= "configuration" Contents
                               [klabel(#KConfiguration), hook(org.kframework.definition.Configuration), latex(  \kconfig#1)]
                     | "rule"    Contents
                               [klabel(#KRule), hook(org.kframework.definition.Rule), latex(  \krule#1)]
                     | "context" Contents
                               [klabel(#KContext), hook(org.kframework.definition.Context), latex(  \kcontext#1)]
  syntax Contents ::= Bubble                        [klabel(#NoAttrs)]
                    | Bubble KAttributesDeclaration [klabel(#Attrs), prefer]
  // The following can still change
  syntax KModuleName ::= r"[A-Z][A-Z\\-]*"    [token]
  syntax KSort       ::= r"[A-Z][A-Za-z0-9]*" [token, hook(org.kframework.definition.Sort)]
endmodule


module E-KORE
  imports OUTER-KORE
  imports BUBBLE
endmodule
