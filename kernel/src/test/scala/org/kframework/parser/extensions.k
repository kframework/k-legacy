extension MULTI-PRODUCTIONS

  syntax KProduction ::= KProductionItems "[" KAttributes "]" [klabel(KProductionWithAttributes)]

  rule KSyntaxProduction(KS:KSort, KProductionWithAttributes(KP:KProductionItems, KA:KAttributes))
    => KSentenceWithAttributes(KSyntaxProduction(KS, KP), KA)

  syntax KProduction ::= KProduction "|" KProduction [klabel(KProductionBlock), assoc]

  rule KSyntaxProduction(KS:KSort, KProductionBlock(KP1:KProduction, KP2:KProduction))
    => KSentenceList(KSyntaxProduction(KS, KP1), KSyntaxProduction(KS, KP2))


  syntax KKeySet ::= "#getKLabels" "(" KAttributes ")" [klabel(KLabelExtractor)]
  syntax KAttributes ::= "#mkPriorityBlock" "(" KProduction ")" [klabel(KAttributeExtractor)]
  syntax KPriority ::= "#mkPriority" "(" KProduction ")" [klabel(KPriorityBuilder)]
  syntax KProduction ::= "#convertPriorities" "(" KProduction ")" [klabel(KPriorityConverter)]




// TODO: How to handle generating the correct "syntax priority ..." clauses?
  syntax KProduction ::= KProduction ">" KProduction [klabel(KProductionPriority), assoc]

  rule KSyntaxProduction(KS:KSort, KProductionPriority(KP1:KProduction, KP2:KProduction))
    => KSentenceList( KSyntaxPriority(KPriorityBuilder(KProductionPriority(KP1, KP2)))
                    , KSyntaxProduction(KS, KPriorityConverter(KProductionPriority(KP1, KP2)))
                    )

  rule KLabelExtractor(KAs:KAttributes) => .KKeySet [otherwise]
  rule KLabelExtractor(KAttributeApply(KToken(klabel, KAttributeKey), KKeyList(KAK:KAttributeKey, .KKeyList)))
    => KAK
  rule KLabelExtractor(KAttributes(KA1:KAttributes, KA2:KAttributes))
    => KKeySet(KLabelExtractor(KA1), KLabelExtractor(KA2))

  rule KAttributeExtractor(KProductionItems(KP1:KProduction, KP2:KProduction))
    => .KAttributes
  rule KAttributeExtractor(KProductionWithAttributes(KP:KProduction, KA:KAttributes))
    => KA
  rule KAttributeExtractor(KProductionBlock(KP1:KProduction, KP2:KProduction))
    => KAttributes(KAttributeExtractor(KP1), KAttributeExtractor(KP2))
  rule KAttributeExtractor(KProductionPriority(KP1:KProduction, KP2:KProduction))
    => KAttributes(KAttributeExtractor(KP1), KAttributeExtractor(KP2))

  rule KPriorityBuilder(KProductionItems(KP1:KProductionItems, KP2:KProductionItems))
    => .KKeySet
  rule KPriorityBuilder(KProductionWithAttributes(KP:KProductionItems, KA:KAttributes))
    => KLabelExtractor(KA)
  rule KPriorityBuilder(KProductionBlock(KP1:KProduction, KP2:KProduction))
    => KLabelExtractor(KAttributes(KAttributeExtractor(KP1), KAttributeExtractor(KP2)))
  rule KPriorityBuilder(KProductionPriority(KP1:KProduction, KP2:KProduction))
    => KPriorityItems(KPriorityBuilder(KP1), KPriorityBuilder(KP2))

  rule KPriorityConverter(KProductionItems(KP1:KProductionItems, KP2:KProductionItems))
    => KProductionItems(KP1, KP2)
  rule KPriorityConverter(KProductionWithAttributes(KP:KProductionItems, KA:KAttributes))
    => KProductionWithAttributes(KP, KA))
  rule KPriorityConverter(KProductionBlock(KP1:KProduction, KP2:KProduction))
    => KProductionBlock(KP1, KP2)
  rule KPriorityConverter(KProductionPriority(KP1:KProduction, KP2:KProduction))
    => KProductionBlock(KPriorityConverter(KP1), KPriorityConverter(KP2))

//  syntax KSyntaxSentence ::= "syntax" KSort "::=" KProductionPriority [klabel(KMultiProduction)]
endextension
