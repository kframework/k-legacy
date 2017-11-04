package org.kframework.minikore

import org.kframework.minikore.MiniKore._
import org.kframework.definition
import org.kframework.kore.KORE

/**
  * Created by lpena on 10/11/16.
  */

object KDefinitionDSL {

  // Productions
  // ===========

  trait ProductionItem
  case class Sort(name: String) extends ProductionItem
  case class Regex(regex: String) extends ProductionItem
  case class Terminal(name: String) extends ProductionItem

  implicit def asTerminal(name: String): Terminal = Terminal(name)

  def productionAsPattern: ProductionItem => Pattern = {
    case Terminal(str)  => application(KoreToMini.iTerminal, str)
    case Sort(sortName) => application(KoreToMini.iNonTerminal, sortName)
    case Regex(str)     => Application(KoreToMini.iRegexTerminal, Seq(Application("#", Nil), Application(str, Nil), Application("#", Nil)))
  }

  def makeCtorString(pis: Seq[Pattern]): String = pis flatMap {
    case Application(KoreToMini.`iNonTerminal`, Application(_, Nil) :: Nil)                                                                => "_"
    case Application(KoreToMini.`iTerminal`, Application(str, Nil) :: followRegex :: Nil)                                                  => str // should take into account the followRegex
    case Application(KoreToMini.`iRegexTerminal`, Application(precede, Nil) :: Application(regex, Nil) :: Application(follow, Nil) :: Nil) => "r\"" + regex + "\"" // should take into account the precede/follow
  } mkString

  // Attributes
  // ==========

  implicit def asAttribute(name: String): Application  = Application(name, Seq.empty)

  def application(label: String, value: String): Application = Application(label, Seq(Application(value, Seq.empty)))
  def klabel(value: String): Application                     = application("klabel", value)
  def kprod(production: Seq[Pattern]): Application           = Application("production", production)

  def getKLabel(atts: Attributes): Option[String] = getAttributeKey("klabel", atts) match {
    case Seq(Seq(Application(value, Nil))) => Some(value)
    case _                                 => None
  }

  // MINIKORE DSL
  // ============

  case class definition(modules: Module*) {
    def att(atts: Pattern*): Definition = Definition(atts, modules)
  }

  case class module(name: String, sentences: Sentence*) {
    def att(atts: Pattern*): Module = Module(name, sentences, atts)
  }

  def imports(name: String) = Import(name, Seq.empty)

  case class symbol(sort: Sort, klabel: String, args: Sort*) {
    def att(atts: Pattern*): SymbolDeclaration = SymbolDeclaration(sort.name, klabel, args map _.name, atts)
  }

  case class syntax(sort: Sort, pis: Seq[ProductionItem] = Seq.empty) {
    def is(pis: ProductionItem*): syntax = syntax(sort, pis)
    def att(atts: Pattern*): SymbolDeclaration = SymbolDeclaration(sort.name, getKLabel(atts).getOrElse(makeCtorString(pis map productionAsPattern)), pis.collect { case Sort(name) => name }, atts :+ kprod(pis map productionAsPattern))
  }

  implicit def asDefinition(d: definition): Definition  = d.att()
  implicit def asModule(m: module): Module              = m.att()
  implicit def asSentence(s: symbol): SymbolDeclaration = s.att()
  implicit def asSentence(s: syntax): SymbolDeclaration = s.att()
}


object KOREDefinition {
  import KDefinitionDSL._


  // ### KBUBBLE
  val KBubbleRegex = "[^ \t\n\r]+"

  val KBubbleItem = Sort("KBubbleItem")
  val KBubble = Sort("KBubble")

  val KBUBBLE: Module = module("KBUBBLE",
    syntax(KBubbleItem) is Regex(KBubbleRegex) att("token", application("reject2", "rule|syntax|endmodule|configuration|context")),

    syntax(KBubble) is (KBubble, KBubbleItem) att "token",
    syntax(KBubble) is KBubbleItem att "token"
  )


  // ### KTOKENS
  val KRegexSymbol        = "[A-Za-z0-9\\.@#\\|\\-]+"
  val KRegexSymbolEscaped = "`[^\n\r\t\f]+`"
  val KRegexString        = "[\"](([^\n\r\t\f\"\\\\])|([\\\\][nrtf\"\\\\])|([\\\\][x][0-9a-fA-F]{2})|([\\\\][u][0-9a-fA-F]{4})|([\\\\][U][0-9a-fA-F]{8}))*[\"]"

  //val KRegexSort = "[A-Z][A-Za-z0-9]*"
  //val KRegexSymbol3 = "(?<![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*"
  // TODO: the (?<! is a signal to the parser that it should be used as a "precedes" clause, do we need it?
  // val KRegexAttributeKey3 = """(?<![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*"""

  val KSymbol = Sort("KSymbol")
  val KString = Sort("KString")

  val KTOKENS: Module = module("KTOKENS",
    syntax(KSymbol) is Regex(KRegexSymbol) att "token",
    syntax(KSymbol) is Regex(KRegexSymbolEscaped) att "token",

    syntax(KString) is Regex(KRegexString) att "token"
  )

  
  // ### KML
  val KMLVariable    = Sort("KMLVariable")
  val KMLPattern     = Sort("KMLPattern")
  val KMLPatternList = Sort("KMLPatternList")

  val KML: Module = module("KML",
    imports("KTOKENS"),

    syntax(KMLVariable) is (KSymbol, ":", KSymbol) att klabel("KMLVariable"),
    syntax(KMLPattern) is KMLVariable,
    syntax(KMLPattern) is KSymbol,

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


  // ### KSENTENCE
  val KTerminal    = Sort("KTerminal")
  val KNonTerminal = Sort("KNonTerminal")

  val KProduction = Sort("KProduction")
  val KPriority   = Sort("KPriority")

  val KAttributes = Sort("KAttributes")

  val KSentence     = Sort("KSentence")
  val KSentenceList = Sort("KSentenceList")

  val KSENTENCE: Module = module("KSENTENCE",
    imports("KML"),
    imports("KBUBBLE"),

    syntax(KTerminal) is KString,
    syntax(KTerminal) is ("r", KString) att klabel("KRegexTerminal"),
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
    syntax(KSentence) is ("syntax", KSymbol, ":=", KMLPattern, KAttributes) att klabel("KSymbolDeclaration"),
    syntax(KSentence) is ("syntax", "priority", KPriority, KAttributes) att klabel("KSyntaxPriority"),
    syntax(KSentence) is ("rule", KBubble, KAttributes) att klabel("KRule"),

    syntax(KSentenceList) is KSentence,
    syntax(KSentenceList) is "" att klabel(".KSentenceList"),
    syntax(KSentenceList) is (KSentence, KSentenceList) att klabel("KSentenceList")
  )


  // ### KDEFINITION
  // val KRegexModuleName = "[A-Z][A-Z\\-]*"
  // val KRequire = Sort("KRequire")
  // val KRequireList = Sort("KRequireList")

  val KModule     = Sort("KModule")
  val KModuleList = Sort("KModuleList")
  val KDefinition = Sort("KDefinition")

  val KDEFINITION: Module = module("KDEFINITION",
    imports("KSENTENCE"),

    syntax(KModule) is ("module", KSymbol, KSentenceList, "endmodule", KAttributes) att klabel("KModule"),
    syntax(KModuleList) is "" att klabel(".KModuleList"),
    syntax(KModuleList) is (KModule, KModuleList) att klabel("KModuleList"),

    syntax(KDefinition) is (KAttributes, KModuleList) att klabel("KDefinition")
  )


  // ### KORE
  val KOREDef = definition(KTOKENS, KBUBBLE, KML, KSENTENCE, KDEFINITION) att (application(KoreToMini.iMainModule, "KDEFINITION"), application(KoreToMini.iEntryModules, "KDEFINITION"))
}
