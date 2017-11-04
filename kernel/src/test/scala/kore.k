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
  syntax KList ::= ".KList"          [klabel(.KList), khook(org.kframework.kore.EmptyKList)]
  syntax KList ::= ".::KList"        [klabel(.KList), khook(org.kframework.kore.EmptyKList)]
  syntax KList ::= KList "," KList   [klabel(KList), left, assoc, unit(.KList), khook(org.kframework.kore.KList), prefer]
endmodule

module KATTRIBUTES
  imports KBASIC

  syntax KAttributeKey ::= r"[\\.A-Za-z\\-0-9]*" [token, khook(org.kframework.kore.KLabel)]
  syntax KAttributeKey ::= r"`(\\\\`|\\\\\\\\|[^`\\\\\n\r])+`" [token, khook(org.kframework.kore.KLabel)]
  syntax KAttributeKey ::= r"(?![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*" [token, khook(org.kframework.kore.KLabel), autoReject]

  syntax KKeyList ::= KAttributeKey
  syntax KKeyList ::= "" [klabel(.KKeyList)]
  syntax KKeyList ::= KKeyList "," KKeyList [klabel(KKeyList)]

  syntax KAttribute ::= KAttributeKey
  syntax KAttribute ::= KAttributeKey "(" KKeyList ")" [klabel(KAttributeApply)]

  syntax KAttributes ::= KAttribute
  syntax KAttributes ::= "" [klabel(.KAttributes)]
  syntax KAttributes ::= KAttributes "," KAttributes [klabel(KAttributes)]

  syntax KBott ::= KAttributes
  syntax KItem ::= KBott [allowChainSubsort]
endmodule

module KML
  imports KSTRING

  syntax KMLVar [klabel(KMLVar)]

  syntax KMLFormula ::= KMLVar
  syntax KMLFormula ::= "tt" [klabel(KMLtrue)]
  syntax KMLFormula ::= "ff" [klabel(KMLfalse)]

  syntax KMLFormula ::= "~" KMLFormula [klabel(KMLnot)]
  syntax KMLFormula ::= KMLFormula "/\\" KMLFormula [klabel(KMLand)]
  syntax KMLFormula ::= KMLFormula "\\/" KMLFormula [klabel(KMLor)]

  syntax KMLFormula ::= "E" KMLVar "." KMLFormula [klabel(KMLexists)]
  syntax KMLFormula ::= "A" KMLVar "." KMLFormula [klabel(KMLforall)]

  syntax KMLFormula ::= KMLFormula "=>" KMLFormula [klabel(KMLnext)]
endmodule

module KSENTENCES
  imports KSTRING
  imports KATTRIBUTES

  syntax KSort ::= r"[A-Z][A-Za-z0-9]*" [token, klabel(KSort)]

  syntax KTerminal ::= KString
  syntax KTerminal ::= "r" KString [klabel(KRegex)]
  syntax KNonTerminal ::= KSort

  syntax KProductionItem ::= KTerminal
  syntax KProductionItem ::= KNonTerminal
  syntax KProduction ::= KProductionItem
  syntax KProduction ::= KProductionItem KProduction [assoc, klabel(KProduction)]

  syntax KPreSentence ::= "syntax" KSort [klabel(KSortDecl)]
  syntax KPreSentence ::= "syntax" KSort "::=" KProduction [klabel(KSyntax)]

  syntax KSentence ::= KPreSentence
  syntax KSentence ::= KPreSentence "[" KAttributes "]" [klabel(KSentence)]

  syntax KSentenceList ::= "" [klabel(.KSentenceList)]
  syntax KSentenceList ::= KSentence KSentenceList [klabel(KSentenceList)]
endmodule

module KDEFINITION
  imports KSENTENCES

  syntax KRequire ::= "require" KString [klabel(KRequire)]
  syntax KRequireList ::= "" [klabel(.KRequireList)]
  syntax KRequireList ::= KRequire KRequireList [klabel(KRequireList)]

  syntax KModuleName ::= r"[A-Z][A-Z\\-]*" [token, klabel(KModuleName)]
  syntax KImport ::= "imports" KModuleName [klabel(KImport)]
  syntax KImportList ::= "" [klabel(.KImportList)]
  syntax KImportList ::= KImport KImportList [klabel(KImportList)]

  syntax KModule ::= "module" KModuleName KImportList KSentenceList "endmodule" [klabel(KModule)]
  syntax KModuleList ::= "" [klabel(.KModuleList)]
  syntax KModuleList ::= KModule KModuleList [klabel(KModuleList)]

  syntax KDefinition ::= KRequireList KModuleList [klabel(KDefinition)]
endmodule
