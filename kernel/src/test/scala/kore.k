module KSTRING
  syntax KString ::= r"[\"](([^\n\r\t\f\"\\\\])|([\\\\][nrtf\"\\\\])|([\\\\][x][0-9a-fA-F]{2})|([\\\\][u][0-9a-fA-F]{4})|([\\\\][U][0-9a-fA-F]{8}))*[\"]" [token, khook(org.kframework.kore.KString)]
endmodule

module KATTRIBUTES
  syntax KAttributeKey ::= r"[\\.A-Za-z\\-0-9]*" [token, khook(org.kframework.kore.KLabel)]
  syntax KAttributeKey ::= r"`(\\\\`|\\\\\\\\|[^`\\\\\n\r\t\f])+`" [token, khook(org.kframework.kore.KLabel)]
  syntax KAttributeKey ::= r"(?![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*" [token, khook(org.kframework.kore.KLabel), autoReject]

  syntax KKeyList ::= KAttributeKey
  syntax KKeyList ::= "" [klabel(.KKeyList)]
  syntax KKeyList ::= KKeyList "," KKeyList [klabel(KKeyList), assoc, kunit(.KKeyList)]

  syntax KKeySet ::= KAttributeKey
  syntax KKeySet ::= "" [klabel(.KKeySet)]
  syntax KKeySet ::= KKeySet KKeySet [klabel(KKeySet), assoc, comm, kunit(.KKeyList)]

  syntax KAttribute ::= KAttributeKey
  syntax KAttribute ::= KAttributeKey "(" KKeyList ")" [klabel(KAttributeApply)]

  syntax KAttributes ::= KAttribute
  syntax KAttributes ::= "" [klabel(.KAttributes)]
  syntax KAttributes ::= KAttribute "," KAttributes [klabel(KAttributes)]
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

  syntax KProduction ::= KTerminal
  syntax KProduction ::= KNonTerminal
  syntax KProduction ::= KProduction KProduction [klabel(KProductionItems), assoc]

  syntax KPriority ::= KKeySet
  syntax KPriority ::= KPriority ">" KPriority [klabel(KPriorityItems), assoc]

  syntax KSentence ::= "syntax" KSort [klabel(KSortDecl)]
  syntax KSentence ::= "syntax" KSort "::=" KProduction [klabel(KProduction)]
  syntax KSentence ::= "syntax" "priority" KPriority [klabel(KPriority)]
  syntax KSentence ::= KSentence "[" KAttributes "]" [klabel(KSentenceWithAttributes)]

  syntax KSentenceList ::= KSentence
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
