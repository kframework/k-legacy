extension MULTI-PRODUCTIONS

  syntax KProduction ::= KProduction "[" KAttributes "]" [klabel(KProductionWithAttributes)]

  rule KSyntaxProduction(KS:KSort, KProductionWithAttributes(KP:KProduction, KA:KAttributes))
    => KSentenceWithAttributes(KSyntaxProduction(KS, KP), KA)

  syntax KProductionBlock ::= KProduction
  syntax KProductionBlock ::= KProduction "|" KProductionBlock [klabel(KProductionBlock)]

  rule KSyntaxProduction(KS:KSort, KProductionBlock(KP:KProduction, KPB:KProductionBlock))
    => KSentenceList(KSyntaxProduction(KS, KP), KSyntaxProduction(KS, KPB))

// TODO: How to handle generating the correct "syntax priority ..." clauses?
//  syntax KProductionPriority ::= KProductionBlock
//  syntax KProductionPriority ::= KProductionBlock ">" KProductionPriority [klabel(KProductionPriority)]
//
//  rule KSyntaxProduction(KS:KSort, KProductionPriority(KPB:KProductionBlock, KPP:KProductionPriority))
//    => KSentenceList(KSyntaxProduction(KS, KPB), KSyntaxProduction(KS, KPP))

  syntax KSyntaxSentence ::= "syntax" KSort "::=" KProductionPriority [klabel(KMultiProduction)]
endextension
