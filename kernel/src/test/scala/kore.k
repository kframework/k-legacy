module KSORT
  syntax K [khook(K.K)]
endmodule

module KBASIC
  imports KSORT

  syntax KLabel
  syntax KItem [khook(K.KItem)]
  syntax KConfigVar
  syntax KBott
  syntax KResult
  syntax MetaVariable
  syntax Bottom

  syntax K ::= KItem  [allowChainSubsort]

  syntax KList ::= K                 [allowChainSubsort]
                 | ".KList"          [klabel(.KList), khook(org.kframework.kore.EmptyKList)]
                 | ".::KList"        [klabel(.KList), khook(org.kframework.kore.EmptyKList)]
                 | KList "," KList   [klabel(KList), left, assoc, unit(.KList), khook(org.kframework.kore.KList), prefer]
endmodule

module KSTRING
  syntax KString ::= r"[\"](([^\n\r\t\f\"\\\\])|([\\\\][nrtf\"\\\\])|([\\\\][x][0-9a-fA-F]{2})|([\\\\][u][0-9a-fA-F]{4})|([\\\\][U][0-9a-fA-F]{8}))*[\"]" [token, khook(org.kframework.kore.KString)]
endmodule

module KATTRIBUTES
  imports KBASIC

  syntax KAttributeKey ::= r"[\\.A-Za-z\\-0-9]*" [token, khook(org.kframework.kore.KLabel)]
                         | r"`(\\\\`|\\\\\\\\|[^`\\\\\n\r\t\f])+`" [token, khook(org.kframework.kore.KLabel)]
                         | r"(?![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*" [token, khook(org.kframework.kore.KLabel), autoReject]

  syntax KKeyList ::= KAttributeKey
                    | "" [klabel(.KKeyList)]
                    | KKeyList "," KKeyList [klabel(KKeyList)]

  syntax KAttribute ::= KAttributeKey
                      | KAttributeKey "(" KKeyList ")" [klabel(KAttributeApply)]

  syntax KAttributes ::= KAttribute
                       | "" [klabel(.KAttributes)]
                       | KAttribute "," KAttributes [klabel(KAttributes)]
endmodule

module KML
  imports KSTRING

  syntax KMLVar [klabel(KMLVar)]

  syntax KMLFormula ::= KMLVar
                      | "tt" [klabel(KMLtrue)]
                      | "ff" [klabel(KMLfalse)]
                      | "~" KMLFormula [klabel(KMLnot)]
                      | KMLFormula "/\\" KMLFormula [klabel(KMLand)]
                      | KMLFormula "\\/" KMLFormula [klabel(KMLor)]
                      | "E" KMLVar "." KMLFormula [klabel(KMLexists)]
                      | "A" KMLVar "." KMLFormula [klabel(KMLforall)]
                      | KMLFormula "=>" KMLFormula [klabel(KMLnext)]
endmodule

module KSENTENCES
  imports KSTRING
  imports KATTRIBUTES

  syntax KSort ::= r"[A-Z][A-Za-z0-9]*" [token, klabel(KSort)]

  syntax KTerminal ::= KString
                     | "r" KString [klabel(KRegex)]
  syntax KNonTerminal ::= KSort

  syntax KProductionItems ::= KTerminal
                            | KNonTerminal
                            | KProductionItems KProductionItems [klabel(KProduction), assoc]

  syntax KProduction ::= KProductionItems
                       | KProductionItems "[" KAttributes "]" [klabel(KProductionWithAttributes)]

  syntax KProductions ::= KProduction
                        | KProductions "|" KProductions [klabel(KProductionBlock), assoc, comm]
                        | KProductions ">" KProductions [klabel(KProductionBlocks), assoc]

  syntax KSentence ::= "syntax" KSort [klabel(KSortDecl)]
                     | "syntax" KSort "[" KAttributes "]" [klabel(KSortDeclWithAttributes)]
                     | "syntax" KSort "::=" KProductions [klabel(KSyntax)]

  syntax KSentenceList ::= KSentence
                         | "" [klabel(.KSentenceList)]
                         | KSentence KSentenceList [klabel(KSentenceList)]
endmodule

module KDEFINITION
  imports KSENTENCES

  syntax KRequire ::= "require" KString [klabel(KRequire)]
  syntax KRequireList ::= "" [klabel(.KRequireList)]
                        | KRequire KRequireList [klabel(KRequireList)]

  syntax KModuleName ::= r"[A-Z][A-Z\\-]*" [token, klabel(KModuleName)]

  syntax KImport ::= "imports" KModuleName [klabel(KImport)]
  syntax KImportList ::= "" [klabel(.KImportList)]
                       | KImport KImportList [klabel(KImportList)]

  syntax KModule ::= "module" KModuleName KImportList KSentenceList "endmodule" [klabel(KModule)]
  syntax KModuleList ::= "" [klabel(.KModuleList)]
                       | KModule KModuleList [klabel(KModuleList)]

  syntax KDefinition ::= KRequireList KModuleList [klabel(KDefinition)]
endmodule
