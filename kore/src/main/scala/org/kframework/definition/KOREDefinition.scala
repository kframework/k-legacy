package org.kframework.minikore

import org.kframework.minikore.MiniKore._

/**
  * Created by lpena on 10/11/16.
  */

object KDefinitionDSL {

  case class module(name: String, sentences: Sentence*) {
    def att(atts: Pattern*): Module = Module(name, sentences, atts)
  }
  implicit def asModule(m: module): Module = m.att()

  def imports(name: String) = Import(name, Seq.empty)

  trait ProductionItem
  case class Sort(name: String) extends ProductionItem
  case class Regex(regex: String) extends ProductionItem
  case class Terminal(name: String) extends ProductionItem

  implicit def asTerminal(name: String): Terminal = Terminal(name)

  // TODO: move to minikore, findAtt should use it
  def getAttributeKey(key: String, atts: Attributes): Seq[Seq[Pattern]] = atts.collect {
    //case dv@DomainValue(str, _) if key == str => dv
    case Application(str, args) if key == str => args
  }

  def getKLabel(atts: Attributes): Option[String] = getAttributeKey("klabel", atts) match {
    case (DomainValue("KSymbol", value) :: _) :: _ => Some(value)
    case _                                         => None
  }

  implicit def productionItemToPatterns(pis: Seq[ProductionItem]): Seq[Pattern] = pis map {
    case Sort(name)     => DomainValue("KSymbol", name)
    case Terminal(name) => DomainValue("KString", name)
    case Regex(str)     => Application("KRegex", Seq(DomainValue("KString", str)))
  }

  def productionInfo(pis: Seq[Pattern]): (String, Seq[String]) = {
    val prodInfo = pis map {
      case DomainValue("KString", str)                             => (str, Seq.empty)
      case DomainValue("KSymbol", sortName)                        => ("_", Seq(sortName))
      case Application("KRegex", DomainValue("KString", str) :: _) => ("r\"" + str + "\"", Seq.empty)
      case _ => throw new Exception
    }
    (prodInfo flatMap (x => x._1) mkString, prodInfo flatMap (x => x._2))
  }

  case class syntax(sort: Sort, pis: Seq[ProductionItem] = Seq.empty) {
    def is(pis: ProductionItem*): syntax = syntax(sort, pis)
    def att(atts: Pattern*): SymbolDeclaration = SymbolDeclaration(sort.name, getKLabel(atts).getOrElse(""), pis.collect { case Sort(name) => name }, atts :+ kprod(productionInfo(pis)._1))
  }

  implicit def asSentence(bs: syntax): SymbolDeclaration = bs.att()

  implicit def asDomainValue(name: String): DomainValue = DomainValue(name, "")

  def klabel(value: String): DomainValue = DomainValue("klabel", value)
  def kprod(production: String): DomainValue = DomainValue("production", production)
  //def khook(value: String): DomainValue = DomainValue("khook", value)
  //def kunit(value: String): DomainValue = DomainValue("unit", value)

}


object KOREDefinition {
  import KDefinitionDSL._


  // ### KBUBBLE
  val KBubbleRegex = "[^ \t\n\r]+"

  val KBubbleItem = Sort("KBubbleItem")
  val KBubble = Sort("KBubble")

  val KBUBBLE: Module = module("KBUBBLE",
    // TODO: Must make the parser actually accept reject2 in this format (as opposed to vertical bars)
    syntax(KBubbleItem) is Regex(KBubbleRegex) att("token", DomainValue("reject2", "rule|syntax|endmodule|configuration|context")),
    syntax(KBubble) is (KBubble, KBubbleItem) att "token",
    syntax(KBubble) is KBubbleItem att "token"
  )


  // ### KTOKENS
  //val KRegexSort = "[A-Z][A-Za-z0-9]*"
  val KRegexSymbol1 = "[\\.A-Za-z\\-0-9]*"
  val KRegexSymbol2 = "`(\\\\`|\\\\\\\\|[^`\\\\\n\r\t\f])+`"
  val KRegexSymbol3 = "(?![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*"
  // TODO: the (?<! is a signal to the parser that it should be used as a "precedes" clause, do we need it?
  // val KRegexAttributeKey3 = """(?<![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*"""

  val KRegexString = "[\"](([^\n\r\t\f\"\\\\])|([\\\\][nrtf\"\\\\])|([\\\\][x][0-9a-fA-F]{2})|([\\\\][u][0-9a-fA-F]{4})|([\\\\][U][0-9a-fA-F]{8}))*[\"]"

  val KSymbol = Sort("KSymbol")
  val KString = Sort("KString")

  val KTOKENS: Module = module("KTOKENS",
    syntax(KSymbol) is Regex(KRegexSymbol1) att "token",
    syntax(KSymbol) is Regex(KRegexSymbol2) att "token",
    syntax(KSymbol) is Regex(KRegexSymbol3) att("token", "autoReject"),
    syntax(KString) is Regex(KRegexString) att "token"
  )

  
  // ### KML
  val KMLVariable = Sort("KMLVariable")
  val KMLPattern = Sort("KMLPattern")
  val KMLPatternList = Sort("KMLPatternList")

  val KML: Module = module("KML",
    imports("KTOKENS"),

    syntax(KMLVariable) is (KSymbol, ":", KSymbol) att klabel("KMLVariable"),
    syntax(KMLPattern) is KMLVariable,
    syntax(KMLPattern) is KSymbol,
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
  )


  // ### KSENTENCES
  val KTerminal = Sort("KTerminal")
  val KNonTerminal = Sort("KNonTerminal")

  val KProduction = Sort("KProduction")
  val KPriority = Sort("KPriority")

  val KAttributes = Sort("KAttributes")

  val KSentence = Sort("KSentence")
  val KSentenceList = Sort("KSentenceList")

  val KSENTENCE: Module = module("KSENTENCE",
    imports("KML"),
    imports("KBUBBLE"),

    syntax(KTerminal) is KString,
    syntax(KTerminal) is ("r", KString) att klabel("KRegex"),
    syntax(KNonTerminal) is KSymbol,

    syntax(KProduction) is KTerminal,
    syntax(KProduction) is KNonTerminal,
    syntax(KProduction) is (KProduction, KProduction) att(klabel("KProduction"), "assoc"),
    syntax(KPriority) is KMLPatternList,
    syntax(KPriority) is (KPriority, ">", KPriority) att(klabel("KPriorityItems"), "assoc"),

    syntax(KAttributes) is "" att klabel(".KAttributes"),
    syntax(KAttributes) is ("[", KMLPatternList, "]") att klabel("KAttributes"),

    syntax(KSentence) is ("imports", KSymbol, KAttributes) att klabel("KImport"),
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
  )


  // ### KDEFINITION
  // val KRegexModuleName = "[A-Z][A-Z\\-]*"
  val KRequire = Sort("KRequire")
  val KRequireList = Sort("KRequireList")

  val KModule = Sort("KModule")
  val KModuleList = Sort("KModuleList")

  val KDefinition = Sort("KDefinition")

  val KDEFINITION: Module = module("KDEFINITION",
    imports("KSENTENCE"),

    syntax(KRequire) is ("require", KString) att klabel("KRequire"),
    syntax(KRequireList) is "" att klabel(".KRequireList"),
    syntax(KRequireList) is (KRequire, KRequireList) att klabel("KRequireList"),

    syntax(KModule) is ("module", KSymbol, KSentenceList, "endmodule", KAttributes) att klabel("KModule"),
    syntax(KModuleList) is "" att klabel(".KModuleList"),
    syntax(KModuleList) is (KModule, KModuleList) att klabel("KModuleList"),

    syntax(KDefinition) is (KRequireList, KModuleList) att klabel("KDefinition")
  ) att KoreToMini.iMainModule


  // ### KORE
  val KOREDef = Definition(Seq(KTOKENS, KBUBBLE, KML, KSENTENCE, KDEFINITION), Seq.empty)
}