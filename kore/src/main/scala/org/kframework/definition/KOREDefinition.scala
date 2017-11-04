package org.kframework.minikore

import org.kframework.minikore.MiniKore._
import org.kframework.definition
import org.kframework.kore.KORE

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
  def getAttributeKey(key: String, atts: Attributes): Seq[Seq[Pattern]] = atts collect {
    //case dv@DomainValue(str, _) if key == str => dv
    case Application(`key`, args) => args
  }

  def getKLabel(atts: Attributes): Option[String] = getAttributeKey("klabel", atts) match {
    case Seq(Seq(Application(value, Nil))) => Some(value)
    case _                                 => None
  }


  // TODO: This representation should probably be changed (to nested applications)
  def productionsAsPatterns(pis: Seq[ProductionItem]): Seq[Pattern] = pis map {
    case Terminal(str)  => application(KoreToMini.iTerminal, str)
    case Sort(sortName) => application(KoreToMini.iNonTerminal, sortName)
    case Regex(str)     => Application(KoreToMini.iRegexTerminal, Seq(Application("#", Nil), Application(str, Nil), Application("#", Nil)))
  }

  def makeCtorString(pis: Seq[Pattern]): String = pis flatMap {
    case Application(KoreToMini.`iNonTerminal`, Seq(Application(_, Nil)))                                                            => "_"
    case Application(KoreToMini.`iTerminal`, Application(str, Nil) :: followRegex :: Nil)                                            => str // should take into account the followRegex
    case Application(KoreToMini.`iRegexTerminal`, Seq(Application(precede, Nil), Application(regex, Nil), Application(follow, Nil))) => "r\"" + regex + "\"" // should take into account the precede/follow
  } mkString
  
  def toMiniKoreEncoding: Pattern => Pattern = {
    case Application(KoreToMini.`iNonTerminal`, Application(str, Nil) :: Nil)      => Application(KoreToMini.iNonTerminal, Seq(DomainValue("S", str)))
    case Application(KoreToMini.`iTerminal`, Application(str, Nil) :: followRegex) => Application(KoreToMini.iTerminal, DomainValue("S", str) :: followRegex)
    case Application(KoreToMini.`iRegexTerminal`, Application(precede, Nil) :: Application(regex, Nil) :: Application(follow, Nil) :: Nil) 
                                                                                   => Application(KoreToMini.iRegexTerminal, Seq(DomainValue("S", precede), DomainValue("S", regex), DomainValue("S", follow)))
    case pattern                                                                   => pattern
  }

  def onAttributesSent(f: Pattern => Pattern): Sentence => Sentence = {
    case Import(name, att)                         => Import(name, att map f)
    case SortDeclaration(sort, att)                => SortDeclaration(sort, att map f)
    case SymbolDeclaration(sort, label, args, att) => SymbolDeclaration(sort, label, args, att map f)
    case Rule(pattern, att)                        => Rule(pattern, att map f)
    case Axiom(pattern, att)                       => Axiom(pattern, att map f)
  }

  def onAttributesSent2(f: Pattern => Pattern, s: Sentence): Sentence = s match {
    case Import(name, att)                         => Import(name, att map f)
    case SortDeclaration(sort, att)                => SortDeclaration(sort, att map f)
    case SymbolDeclaration(sort, label, args, att) => SymbolDeclaration(sort, label, args, att map f)
    case Rule(pattern, att)                        => Rule(pattern, att map f)
    case Axiom(pattern, att)                       => Axiom(pattern, att map f)
  }
  
  def onAttributesMod(f: Pattern => Pattern): Module => Module = {
    case Module(name, sentences, att) => Module(name, sentences map onAttributesSent(f), att map f)
  }
  
  def onAttributesDef(f: Pattern => Pattern): Definition => Definition = {
    case Definition(modules, att) => Definition(modules map onAttributesMod(f), att map f)
  }

  case class syntax(sort: Sort, pis: Seq[ProductionItem] = Seq.empty) {
    def is(pis: ProductionItem*): syntax = syntax(sort, pis)
    def att(atts: Pattern*): SymbolDeclaration = SymbolDeclaration(sort.name, getKLabel(atts).getOrElse(makeCtorString(productionsAsPatterns(pis))), pis.collect { case Sort(name) => name }, atts :+ kprod(productionsAsPatterns(pis)))
  }
  implicit def asSentence(bs: syntax): SymbolDeclaration = bs.att()

  implicit def asApplication(name: String): Application = Application(name, Seq.empty)
  def application(label: String, value: String): Application = Application(label, Seq(Application(value, Seq.empty)))

  def klabel(value: String): Application = application("klabel", value)
  def kprod(production: Seq[Pattern]): Application = Application("production", production)
  //def khook(value: String): DomainValue = DomainValue("khook", value)
  //def kunit(value: String): DomainValue = DomainValue("unit", value)

  //application(key, value) // maybe should be this
  def attribute(key: String, value: String): Application = Application(key, Seq(KoreToMini.S(value)))
  def symbol(str: String): DomainValue                   = DomainValue("KSymbol@TOKENS", str)

}


object KOREDefinition {
  import KDefinitionDSL._


  // ### KBUBBLE
  val KBubbleRegex = "[^ \t\n\r]+"

  val KBubbleItem = Sort("KBubbleItem")
  val KBubble = Sort("KBubble")

  val KBUBBLE: Module = module("KBUBBLE",
    // TODO: Must make the parser actually accept reject2 in this format (as opposed to vertical bars)
    syntax(KBubbleItem) is Regex(KBubbleRegex) att("token", application("reject2", "rule|syntax|endmodule|configuration|context")),
    syntax(KBubble) is (KBubble, KBubbleItem) att "token",
    syntax(KBubble) is KBubbleItem att "token"
  )


  // ### KTOKENS
  //val KRegexSort = "[A-Z][A-Za-z0-9]*"
  val KRegexSymbol = "[A-Za-z0-9\\.@#\\|\\-]+"
  //val KRegexSymbolEscaped = "`(\\\\`|\\\\\\\\|[^`\\\\\n\r\t\f])+`"
  //val KRegexSymbol3 = "(?<![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*"
  // TODO: the (?<! is a signal to the parser that it should be used as a "precedes" clause, do we need it?
  // val KRegexAttributeKey3 = """(?<![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*"""

  val KRegexString = "[\"](([^\n\r\t\f\"\\\\])|([\\\\][nrtf\"\\\\])|([\\\\][x][0-9a-fA-F]{2})|([\\\\][u][0-9a-fA-F]{4})|([\\\\][U][0-9a-fA-F]{8}))*[\"]"

  val KSymbol = Sort("KSymbol")
  val KString = Sort("KString")

  val KTOKENS: Module = module("KTOKENS",
    syntax(KSymbol) is Regex(KRegexSymbol) att "token",
    //syntax(KSymbol) is Regex(KRegexSymbol2) att "token",
    //syntax(KSymbol) is Regex(KRegexSymbol3) att("token", "autoReject"),
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


  // ### KSENTENCE
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
    // TODO: Why doesn't this work?
    //syntax(KSentenceList) is (KSentenceList, KSentenceList) att(klabel("KSentenceList"), "assoc", "comm", kunit(".KSentenceList"))
  )


  // ### KDEFINITION
  // val KRegexModuleName = "[A-Z][A-Z\\-]*"
  // val KRequire = Sort("KRequire")
  // val KRequireList = Sort("KRequireList")

  val KModule = Sort("KModule")
  val KModuleList = Sort("KModuleList")

  val KDefinition = Sort("KDefinition")

  val KDEFINITION: Module = module("KDEFINITION",
    imports("KSENTENCE"),

    // syntax(KRequire) is ("require", KString) att klabel("KRequire"),
    // syntax(KRequireList) is "" att klabel(".KRequireList"),
    // syntax(KRequireList) is (KRequire, KRequireList) att klabel("KRequireList"),

    syntax(KModule) is ("module", KSymbol, KSentenceList, "endmodule", KAttributes) att klabel("KModule"),
    syntax(KModuleList) is "" att klabel(".KModuleList"),
    syntax(KModuleList) is (KModule, KModuleList) att klabel("KModuleList"),

    syntax(KDefinition) is (KAttributes, KModuleList) att klabel("KDefinition")
  )


  // ### KORE
  val KOREDef = Definition(Seq(KTOKENS, KBUBBLE, KML, KSENTENCE, KDEFINITION),
    Seq(attribute(KoreToMini.iMainModule, "KDEFINITION"), attribute(KoreToMini.iEntryModules, "KDEFINITION")))


}
