package org.kframework.definition

import org.kframework.attributes.Att
import org.kframework.kore.ADT._
import org.kframework.kore._

/**
  * Created by lpena on 10/11/16.
  */

object KDefinitionDSL {
  def asKApply(label: String, values: List[String]): K =
    KORE.KApply(KORE.KLabel(label), KORE.KList(values map { value => KORE.KToken(value, ADT.SortLookup("AttributeValue"), Att()) }), Att())
  def asKApply(label: String, values: String*): K = asKApply(label, values toList)

  implicit def asAttribute(str: String): K = asKApply(str, List.empty)
  implicit def asNonTerminal(s: ADT.SortLookup): NonTerminal = NonTerminal(s)
  implicit def asTerminal(s: String): Terminal = Terminal(s)
  implicit def asProduction(ps: ProductionItem*): Seq[ProductionItem] = ps
  implicit def asSentence(bp: BecomingSyntax): Sentence = Production(bp.sort, bp.pis, Att())
  implicit def asSentence(bp: BecomingSyntaxSort): Sentence = SyntaxSort(bp.sort, Att())

  def Sort(s: String): ADT.SortLookup = ADT.SortLookup(s)

  def regex(s: String): ProductionItem = RegexTerminal("#", s, "#")

  case class syntax(s: ADT.SortLookup) {
    def is(pis: ProductionItem*): BecomingSyntax = BecomingSyntax(s, pis)
  }
  case class BecomingSyntax(sort: ADT.SortLookup, pis: Seq[ProductionItem]) {
    def att(atts: K*): Production = Production(sort, pis, atts.foldLeft(Att())(_+_))
  }

  def sort(sort: ADT.SortLookup): BecomingSyntaxSort = BecomingSyntaxSort(sort)
  case class BecomingSyntaxSort(sort: ADT.SortLookup) {
    def att(atts: K*): SyntaxSort = SyntaxSort(sort, atts.foldLeft(Att())(_+_))
  }

  def term(label: String, args: K*): K = KApply(KLabelLookup(label), KList(args.toList), Att())
  def rule(lhs: K, rhs: K): Rule = Rule(ADT.KRewrite(lhs, rhs), KORE.KToken("tt", ADT.SortLookup("KMLFormula")), KORE.KToken("tt", ADT.SortLookup("KMLFormula")))

  def >(labels: String*): Set[Tag] = labels map Tag toSet
  def priority(labels: Set[Tag]*): SyntaxPriority = SyntaxPriority(labels)

  def imports(s: Module*): Set[Module] = s toSet
  def sentences(s: Sentence*): Set[Sentence] = s toSet

  def khook(label: String): K = asKApply("khook", List(label))
  def klabel(label: String): K = asKApply("klabel", List(label))
  def ktoken(label: String): K = asKApply("ktoken", List(label))
  def kunit(label: String): K = asKApply("unit", label)
}


object KOREDefinition {
  import KDefinitionDSL._


  // ### KBUBBLE
  val KBubbleRegex = "[^ \t\n\r]+"

  val KBubbleItem = Sort("KBubbleItem")
  val KBubble = Sort("KBubble")

  val KBUBBLE = Module("KBUBBLE", imports(), sentences(
    // TODO: Must make the parser actually accept reject2 in this format (as opposed to vertical bars)
    syntax(KBubbleItem) is regex(KBubbleRegex) att("token", asKApply("reject2", "rule|syntax|endmodule|configuration|context")),
    syntax(KBubble) is (KBubble, KBubbleItem) att "token",
    syntax(KBubble) is KBubbleItem att "token"
  ))


  // ### KTOKENS
  //val KRegexSort = "[A-Z][A-Za-z0-9]*"
  val KRegexSymbol1 = "[\\.A-Za-z\\-0-9]*"
  val KRegexSymbol2 = "`(\\\\`|\\\\\\\\|[^`\\\\\n\r\t\f])+`"
  val KRegexSymbol3 = "(?![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*"
  // TODO: the (?<! is a signal to the parser that it should be used as a "precedes" clause, do we need it?
  // val KRegexAttributeKey3 = """(?<![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*"""

  val KRegexString = "[\"](([^\n\r\t\f\"\\\\])|([\\\\][nrtf\"\\\\])|([\\\\][x][0-9a-fA-F]{2})|([\\\\][u][0-9a-fA-F]{4})|([\\\\][U][0-9a-fA-F]{8}))*[\"]"

  val KSymbol = Sort("KSymbol")
  //val KSort = Sort("KSort")
  val KString = Sort("KString")

  val KTOKENS = Module("KTOKENS", imports(), sentences(
    syntax(KSymbol) is regex(KRegexSymbol1) att "token",
    syntax(KSymbol) is regex(KRegexSymbol2) att "token",
    syntax(KSymbol) is regex(KRegexSymbol3) att("token", "autoReject"),
    //syntax(KSort) is regex(KRegexSort) att("token", klabel("KSort")),
    syntax(KString) is regex(KRegexString) att "token"
  ))

  
  // ### KML
  val KMLVariable = Sort("KMLVariable")
  val KMLPattern = Sort("KMLPattern")
  val KMLPatternList = Sort("KMLPatternList")

  val KML = Module("KML", imports(KTOKENS), sentences(

    syntax(KMLVariable) is (KSymbol, ":", KSymbol) att klabel("KMLVariable"),
    syntax(KMLPattern) is KMLVariable,
    // every <SORT> should have a production like this for variables
    // syntax <SORT> ::= KID ":" "<SORT>"

    syntax(KMLPattern) is "tt" att klabel("KMLTrue"),
    syntax(KMLPattern) is "ff" att klabel("KMLFalse"),

    syntax(KMLPattern) is (KMLPattern, "/\\", KMLPattern) att klabel("KMLAnd"),
    syntax(KMLPattern) is (KMLPattern, "\\/", KMLPattern) att klabel("KMLOr"),
    syntax(KMLPattern) is ("~", KMLPattern) att klabel("KMLNot"),

    syntax(KMLPattern) is (KMLPattern, "->", KMLPattern) att klabel("KMLImplies"),
    syntax(KMLPattern) is ("E", KMLVariable, ".", KMLPattern) att klabel("KMLExists"),
    syntax(KMLPattern) is ("A", KMLVariable, ".", KMLPattern) att klabel("KMLForAll"),

    syntax(KMLPattern) is ("next", KMLPattern) att klabel("KMLNext"),
    syntax(KMLPattern) is (KMLPattern, "=>", KMLPattern) att klabel("KMLRewrite"),
    syntax(KMLPattern) is (KMLPattern, "==", KMLPattern) att klabel("KMLEquals"),

    syntax(KMLPatternList) is "" att klabel(".KMLPatternList"),
    syntax(KMLPatternList) is KMLPattern,
    syntax(KMLPatternList) is (KMLPattern, ",", KMLPatternList) att klabel("KMLPatternList"),

    syntax(KMLPattern) is (KSymbol, "(", KMLPatternList, ")") att klabel("KMLApplication"),
    syntax(KMLPattern) is ("val", "(", KSymbol, ",", KString, ")") att klabel("KMLDomainValue")
  ))


//  // ### KATTRIBUTES
//
//
//  val KAttributeKey = Sort("KAttributeKey")
//  val KKeyList = Sort("KKeyList")
//  val KKeySet = Sort("KKeySet")
//  val KAttribute= Sort("KAttribute")
//  val KAttributes= Sort("KAttributes")
//
//  val KATTRIBUTES = Module("KATTRIBUTES", imports(KML), sentences(
//
//    syntax
//    syntax(KKeyList) is KAttributeKey,
//    syntax(KKeyList) is "" att klabel(".KKeyList"),
//    syntax(KKeyList) is (KKeyList, ",", KKeyList) att(klabel("KKeyList"), "assoc", kunit(".KKeyList")),
//
//    syntax(KKeySet) is KAttributeKey,
//    syntax(KKeySet) is "" att klabel(".KKeySet"),
//    syntax(KKeySet) is (KKeySet, KKeySet) att(klabel("KKeySet"), "assoc", "comm", kunit(".KKeyList")),
//
//    syntax(KAttribute) is KAttributeKey,
//    syntax(KAttribute) is (KAttributeKey, "(", KKeyList, ")") att klabel("KAttributeApply"),
//
//    syntax(KAttributes) is KAttribute,
//    syntax(KAttributes) is "" att klabel(".KAttributes"),
//    syntax(KAttributes) is (KAttributes, ",", KAttributes) att(klabel("KAttributes"), "assoc", "comm", kunit(".KAttributes"))
//  ))
//


  // ### KSENTENCES
  val KTerminal = Sort("KTerminal")
  val KNonTerminal = Sort("KNonTerminal")

  val KProduction = Sort("KProduction")
  val KPriority = Sort("KPriority")

  val KAttributes = Sort("KAttributes")

  val KSentence = Sort("KSentence")
  val KSentenceList = Sort("KSentenceList")

  val KSENTENCE = Module("KSENTENCE", imports(KML, KBUBBLE), sentences(

    syntax(KTerminal) is KString,
    syntax(KTerminal) is ("r", KString) att klabel("KRegex"),
    syntax(KNonTerminal) is KSymbol,

    syntax(KProduction) is KTerminal,
    syntax(KProduction) is KNonTerminal,
    syntax(KProduction) is (KProduction, KProduction) att(klabel("KProduction"), "assoc"),
    syntax(KPriority) is KMLPatternList,
    syntax(KPriority) is (KPriority, ">", KPriority) att(klabel("KPriorityItems"), "assoc"),

    syntax(KAttributes) is ("[", KMLPatternList, "]") att klabel("KAttributes"),

    syntax(KSentence) is ("imports", KSymbol) att klabel("KImport"),
    syntax(KSentence) is ("syntax", KSymbol, KAttributes) att klabel("KSortDeclaration"),
    syntax(KSentence) is ("syntax", KSymbol, "::=", KProduction, KAttributes) att klabel("KSyntaxProduction"),
    syntax(KSentence) is ("syntax", KSymbol, "::=", KMLPattern, KAttributes) att klabel("KSymbolDeclaration"),
    syntax(KSentence) is ("syntax", "priority", KPriority, KAttributes) att klabel("KSyntaxPriority"),
    syntax(KSentence) is ("rule", KBubble, KAttributes) att klabel("KRule"),

    syntax(KSentenceList) is KSentence,
    syntax(KSentenceList) is "" att klabel(".KSentenceList"),
    syntax(KSentenceList) is (KSentence, KSentenceList) att klabel("KSentenceList")
    // TODO: Why doesn't this work?
    //syntax(KSentenceList) is (KSentenceList, KSentenceList) att(klabel("KSentenceList"), "assoc", "comm", kunit(".KSentenceList"))
  ))


  // ### KDEFINITION
  // val KRegexModuleName = "[A-Z][A-Z\\-]*"

  val KRequire = Sort("KRequire")
  val KRequireList = Sort("KRequireList")

  val KModule = Sort("KModule")
  val KModuleList = Sort("KModuleList")

  val KDefinition = Sort("KDefinition")

  val KDEFINITION = Module("KDEFINITION", imports(KSENTENCE), sentences(
    syntax(KRequire) is ("require", KString) att klabel("KRequire"),
    syntax(KRequireList) is "" att klabel(".KRequireList"),
    syntax(KRequireList) is (KRequire, KRequireList) att klabel("KRequireList"),

    syntax(KModule) is ("module", KSymbol, KSentenceList, "endmodule") att klabel("KModule"),
    syntax(KModuleList) is "" att klabel(".KModuleList"),
    syntax(KModuleList) is (KModule, KModuleList) att klabel("KModuleList"),

    syntax(KDefinition) is (KRequireList, KModuleList) att klabel("KDefinition")
  ))


  // ### KORE
  val KOREDef = Map( "KTOKENS" -> KTOKENS
                   , "KBUBBLE" -> KBUBBLE
                   , "KML" -> KML
                   , "KSENTENCE" -> KSENTENCE
                   , "KDEFINITION" -> KDEFINITION
                   )
}
