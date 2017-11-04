package org.kframework.definition

import org.apache.commons.lang3.StringEscapeUtils
import org.kframework.attributes.Att
import org.kframework.builtin.Sorts
import org.kframework.kore.ADT._
import org.kframework.kore._

import collection.JavaConverters._

/**
  * Created by lpena on 10/11/16.
  */

object KDefinitionDSL {

  def asKApply(label: String, values: List[String]): K =
    KORE.KApply(KORE.KLabel(label), KORE.KList(values map { value => KORE.KToken(value, Sorts.KString, Att()) }), Att())
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

  def >(labels: String*): Set[Tag] = labels map Tag toSet
  def priority(labels: Set[Tag]*): SyntaxPriority = SyntaxPriority(labels)

  def imports(s: Module*): Set[Module] = s toSet
  def sentences(s: Sentence*): Set[Sentence] = s toSet
  def khook(label: String): K = asKApply("khook", List(label))
  def klabel(label: String): K = asKApply("klabel", List(label))
  def ktoken(label: String): K = asKApply("ktoken", List(label))
  def kunit(label: String): K = asKApply("unit", label)
//  implicit def asAttribute(str: String): K = asKApply(str, List.empty)
//  implicit def asNonTerminal(s: ADT.SortLookup): NonTerminal = NonTerminal(s)
//  implicit def asTerminal(s: String): Terminal = Terminal(s)
//  implicit def syntaxAsSyntaxSort(syn: syntax): Set[SyntaxSort] = Set(SyntaxSort(syn.sort, Att()))
//  //implicit def asProduction(ps: ProductionItem*): Seq[ProductionItem] = ps
//  //implicit def asSortDecl(bs: BecomingSort): Set[SyntaxSort] = bs.att()
//
//  case class BecomingSyntax(production: Seq[ProductionItem], atts: Att = Att()) {
//    def att(newAtts: K*): BecomingSyntax = BecomingSyntax(production, newAtts.foldLeft(atts)(_+_))
//    def toSentence(sort: ADT.SortLookup): Production = Production(sort, production, atts)
//  }
//  case class syntax(sort: ADT.SortLookup) {
//    def apply(bps: BecomingProduction*): Set[Production] = (bps toSet) map ((x:BecomingProduction) => x.toSentence(sort))
//    def att(atts: K*): Set[SyntaxSort] = Set(SyntaxSort(sort, atts.foldLeft(Att())(_+_)))
//  }
//
//  def |(production: ProductionItem*): BecomingProduction = BecomingProduction(production)
//  case class BecomingProduction(production: Seq[ProductionItem], atts: Att = Att()) {
//    def att(newAtts: K*): BecomingProduction = BecomingProduction(production, newAtts.foldLeft(atts)(_+_))
//    def toSentence(sort: ADT.SortLookup): Production = Production(sort, production, atts)
//  }
//
//  // case class syntax2 ( sort: ADT.SortLookup
//  //                    , sealedPriorityProductions: Seq[Set[BecomingProduction]]
//  //                    , unsealedCurrentPriority: Set[BecomingProduction]
//  //                    , currentProduction: Seq[ProductionItem]
//  //                    , atts: Att = Att()
//  //                    ) {
//  //   def att(newAtts: K*): syntax2 = sealedPriorityProductions match {
//  //     case Seq() =>
//  //   }
//  //   def |(production: ProductionItem*): syntax2 = {
//  //     val pSize = productions.size
//  //     productions match {
//  //       case Seq() => syntax2(sort, Seq(Set((production, Att()))))
//  //       case _     => syntax2(sort, productions(0, pSize-1) ++ (productions(pSize-1) ++ (production, Att())))
//  //     }
//  //   }
//  //   def att(newAtts: K*):
//  // }
//
//  //case class BecomingSort(sort: ADT.SortLookup, atts: Att = Att()) {
//  //  def att(atts: K*): Set[SyntaxSort] = Set(SyntaxSort(sort, atts.foldLeft(Att())(_+_)))
//  //}
//  //def sort(sort: ADT.SortLookup): BecomingSort = BecomingSort(sort)
//
//  def >(labels: String*): Set[Tag] = labels map Tag toSet
//  def priority(labels: Set[Tag]*): Set[SyntaxPriority] = Set(SyntaxPriority(labels))
//
//  def Sort(s: String): ADT.SortLookup = ADT.SortLookup(s)
//  def imports(s: Module*): Set[Module] = s toSet
//  def sentences(s: Set[_ <: Sentence]*): Set[Sentence] = s.flatten.toSet
//  def regex(s: String): ProductionItem = RegexTerminal("#", s, "#")
//
//  def khook(label: String): K = asKApply("khook", List(label))
//  def klabel(label: String): K = asKApply("klabel", List(label))
//  def ktoken(label: String): K = asKApply("ktoken", List(label))
//  def kunit(label: String): K = asKApply("unit", label)
}

object ExpDefinition {
  import KDefinitionDSL._

  val Exp = Sort("Exp")
  val EXP = Module("EXP", imports(), sentences(
    syntax(Exp) is (Exp, "+", Exp) att(klabel("p"), "plus"),
    syntax(Exp) is (Exp, "-", Exp) att("minus", klabel("m")),
    syntax(Exp) is (Exp, "*", Exp) att(klabel("t"), "times"),
    syntax(Exp) is (Exp, "/", Exp) att(klabel("d"), "div"),
    priority( >("p"), >("m", "t"), >("d"))
  ))
}

object KoreDefintion {
  import KDefinitionDSL._


  // ### KSORT
  val K = Sort("K")

  val KSORT = Module("KSORT", imports(), sentences(
    sort(K) att khook("K.K")
  ))


  // ### KBASIC
  val KLabel = Sort("KLabel")
  val KItem = Sort("KItem")
  val KConfigVar = Sort("KConfigVar")
  val KBott = Sort("KBott")
  val KList = Sort("KList")
  val KResult = Sort("KResult")
  val MetaVariable = Sort("MetaVariable")
  val Bottom = Sort("Bottom")

  val KBASIC = Module("KBASIC", imports(KSORT), sentences(
    sort(KLabel),
    sort(KItem) att khook("K.KItem"),
    sort(KConfigVar),
    sort(KBott),
    sort(KResult),
    sort(MetaVariable),
    sort(Bottom),

    syntax(K) is KItem att "allowChainSubsort",

    syntax(KList) is K att "allowChainSubsort",
    syntax(KList) is ".KList" att(klabel(".KList"), khook("org.kframework.kore.EmptyKList")),
    syntax(KList) is ".::KList" att(klabel(".KList"), khook("org.kframework.kore.EmptyKList")),
    syntax(KList) is (KList, ",", KList) att(klabel("KList"), "left", "assoc", kunit(".KList"), khook("org.kframework.kore.KList"), "prefer")
  ))


  // ### KATTRIBUTES
  // TODO: Figure out these regexs.
  val KRegexAttributeKey1 = "[\\.A-Za-z\\-0-9]*"
  val KRegexAttributeKey2 = "`(\\\\`|\\\\\\\\|[^`\\\\\n\r\t\f])+`"
  val KRegexAttributeKey3 = "(?![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*"
  // val KRegexAttributeKey3 = """(?<![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*"""
  // the (?<! is a signal to the parser that it should be used as a "precedes" clause, do we need it?

  val KAttributeKey = Sort("KAttributeKey")
  val KKeyList = Sort("KKeyList")
  val KAttribute= Sort("KAttribute")
  val KAttributes= Sort("KAttributes")

  val KATTRIBUTES = Module("KATTRIBUTES", imports(KBASIC), sentences(
    syntax(KAttributeKey) is regex(KRegexAttributeKey1) att("token", khook("org.kframework.kore.KLabel")),
    syntax(KAttributeKey) is regex(KRegexAttributeKey2) att("token", khook("org.kframework.kore.KLabel")),
    syntax(KAttributeKey) is regex(KRegexAttributeKey3) att("token", khook("org.kframework.kore.KLabel"), "autoReject"),

    syntax(KKeyList) is KAttributeKey,
    syntax(KKeyList) is "" att klabel(".KKeyList"),
    syntax(KKeyList) is (KKeyList, ",", KKeyList) att klabel("KKeyList"),

    syntax(KAttribute) is KAttributeKey,
    syntax(KAttribute) is (KAttributeKey, "(", KKeyList, ")") att klabel("KAttributeApply"),

    syntax(KAttributes) is KAttribute,
    syntax(KAttributes) is "" att klabel(".KAttributes"),
    syntax(KAttributes) is (KAttribute, ",", KAttributes) att klabel("KAttributes")
  ))


  // ### KSTRING
  // TODO: Fix this regex
  val KRegexString = "[\"](([^\n\r\t\f\"\\\\])|([\\\\][nrtf\"\\\\])|([\\\\][x][0-9a-fA-F]{2})|([\\\\][u][0-9a-fA-F]{4})|([\\\\][U][0-9a-fA-F]{8}))*[\"]"

  val KString = Sort("KString")

  val KSTRING = Module("KSTRING", imports(), sentences(
    syntax(KString) is regex(KRegexString) att("token", khook("org.kframework.kore.KString"))
  ))


  // ### KML
  val KMLVar = Sort("KMLVar")
  val KMLFormula = Sort("KMLFormula")
  val KML = Module("KML", imports(KSTRING), sentences(
    sort(KMLVar) att klabel("KMLVar"),

    syntax(KMLFormula) is KMLVar,
    syntax(KMLFormula) is "tt" att klabel("KMLtrue"),
    syntax(KMLFormula) is "ff" att klabel("KMLfalse"),

    syntax(KMLFormula) is ("~", KMLFormula) att klabel("KMLnot"),
    syntax(KMLFormula) is (KMLFormula, "/\\", KMLFormula) att klabel("KMLand"),
    syntax(KMLFormula) is (KMLFormula, "\\/", KMLFormula) att klabel("KMLor"),

    syntax(KMLFormula) is ("E", KMLVar, ".", KMLFormula) att klabel("KMLexists"),
    syntax(KMLFormula) is ("A", KMLVar, ".", KMLFormula) att klabel("KMLforall"),

    syntax(KMLFormula) is (KMLFormula, "=>", KMLFormula) att klabel("KMLnext")
  ))


  // ### KSENTENCES
  val KRegexSort = "[A-Z][A-Za-z0-9]*"

  val KSort = Sort("KSort")

  val KTerminal = Sort("KTerminal")
  val KNonTerminal = Sort("KNonTerminal")

  val KProductionItems = Sort("KProductionItems")
  val KProduction = Sort("KProduction")
  val KProductions = Sort("KProductions")

  val KSentence = Sort("KSentence")
  val KSentenceList = Sort("KSentenceList")

  val KSENTENCES = Module("KSENTENCES", imports(KSTRING, KATTRIBUTES), sentences(
    syntax(KSort) is regex(KRegexSort) att("token", klabel("KSort")),

    syntax(KTerminal) is KString,
    syntax(KTerminal) is ("r", KString) att klabel("KRegex"),
    syntax(KNonTerminal) is KSort,

    syntax(KProductionItems) is KTerminal,
    syntax(KProductionItems) is KNonTerminal,
    syntax(KProductionItems) is (KProductionItems, KProductionItems) att(klabel("KProduction"), "assoc"),

    syntax(KProduction) is KProductionItems,
    syntax(KProduction) is (KProductionItems, "[", KAttributes, "]") att klabel("KProductionWithAttributes"),

    syntax(KProductions) is KProduction,
    syntax(KProductions) is (KProductions, "|", KProductions) att(klabel("KProductionBlock"), "assoc", "comm"),
    syntax(KProductions) is (KProductions, ">", KProductions) att(klabel("KProductionBlocks"), "assoc"),

    syntax(KSentence) is ("syntax", KSort) att klabel("KSortDecl"),
    syntax(KSentence) is ("syntax", KSort, "[", KAttributes, "]") att klabel("KSortDeclWithAttributes"),
    syntax(KSentence) is ("syntax", KSort, "::=", KProductions) att klabel("KSyntax"),

    syntax(KSentenceList) is "" att klabel(".KSentenceList"),
    syntax(KSentenceList) is (KSentence, KSentenceList) att klabel("KSentenceList")

//    priority( >("KRegex")
//            , >("KString")
//            , >("KSort")
//            , >("KSortDecl", "KSyntax")
//            , >("KSentence")
//            , >("KSentenceList")
//            )
  ))


  // ### KDEFINITION
  val KRegexModuleName = "[A-Z][A-Z\\-]*"

  val KRequire = Sort("KRequire")
  val KRequireList = Sort("KRequireList")

  val KModuleName = Sort("KModuleName")
  val KImport = Sort("KImport")
  val KImportList = Sort("KImportList")

  val KModule = Sort("KModule")
  val KModuleList = Sort("KModuleList")

  val KDefinition = Sort("KDefinition")

  val KDEFINITION = Module("KDEFINITION", imports(KSENTENCES), sentences(
    syntax(KRequire) is ("require", KString) att klabel("KRequire"),
    syntax(KRequireList) is "" att klabel(".KRequireList"),
    syntax(KRequireList) is (KRequire, KRequireList) att klabel("KRequireList"),

    syntax(KModuleName) is regex(KRegexModuleName) att("token", klabel("KModuleName")),
    syntax(KImport) is ("imports", KModuleName) att klabel("KImport"),
    syntax(KImportList) is "" att klabel(".KImportList"),
    syntax(KImportList) is (KImport, KImportList) att klabel("KImportList"),

    syntax(KModule) is ("module", KModuleName, KImportList, KSentenceList, "endmodule") att klabel("KModule"),
    syntax(KModuleList) is "" att klabel(".KModuleList"),
    syntax(KModuleList) is (KModule, KModuleList) att klabel("KModuleList"),

    syntax(KDefinition) is (KRequireList, KModuleList) att klabel("KDefinition")
  ))


  // ### KORE
  val KORE = Map( "KSORT" -> KSORT
                , "KBASIC" -> KBASIC
                , "KSTRING" -> KSTRING
                , "KATTRIBUTES" -> KATTRIBUTES
                , "KML" -> KML
                , "KSENTENCES" -> KSENTENCES
                , "KDEFINITION" -> KDEFINITION
                )
}

object KoreDefinitionDown {
  import KDefinitionDSL._
  import KoreDefintion._
  import ADT.KList

  def downKKeyList(parsedKKeyList: K): List[String] = parsedKKeyList match {
    case KApply(KLabelLookup("KKeyList"), KList(list1 :: list2 :: _), _) => downKKeyList(list1) ++ downKKeyList(list2)
    case KToken(att, KAttributeKey, _) => List(att)
    case _ => List.empty
  }

  def downAttributes(parsedAttributes: K): Att = parsedAttributes match {
    case KApply(KLabelLookup("KAttributes"), KList(atts1 :: atts2 :: _), _) => downAttributes(atts1) ++ downAttributes(atts2)
    case KApply(KLabelLookup("KAttributeApply"), KList(KToken(fnc, KAttributeKey, _) :: keyList :: _), _) => Att(asKApply(fnc, downKKeyList(keyList)))
    case KToken(attName, KAttributeKey, _) => Att(attName)
    case _ => Att()
  }

  def downProductionItems(parsedProduction: K): Seq[ProductionItem] = parsedProduction match {
    case KApply(KLabelLookup("KProduction"), KList(productionItems), _) => productionItems flatMap downProductionItems
    case KApply(KLabelLookup("KRegex"), KList(KToken(str, KString, _) :: _), _) => Seq(RegexTerminal("#", str, "#"))
    case KToken(sortName, KSort, _) => Seq(NonTerminal(Sort(sortName)))
    case KToken(str, KString, _) => Seq(Terminal(str))
    case _ => Seq.empty
  }

  def getKLabel(parsedAttributes: K): String = parsedAttributes match {
    case KApply(KLabelLookup("KAttributes"), KList(atts1 :: atts2 :: _), _) => getKLabel(atts1) + getKLabel(atts2)
    case KApply(KLabelLookup("KAttributeApply"), KList(KToken("klabel", KAttributeKey, _) :: KToken(label, KAttributeKey, _) :: _), _) => label
    case _ => ""
  }

  def downPrioritiesHelp(pBlocks: List[K]): List[Seq[String]] = pBlocks match {
    case KApply(KLabelLookup("KProductionBlocks"), KList(productionBlocks), _) :: rest => downPrioritiesHelp(productionBlocks) ++   downPrioritiesHelp(rest)
    case KApply(KLabelLookup("KProductionBlock"), KList(productionBlocks), _) :: rest => List(downPrioritiesHelp(productionBlocks).flatten) ++ downPrioritiesHelp(rest)
    case KApply(KLabelLookup("KProductionWithAttributes"), KList(prods :: atts :: _), _) :: rest => Seq(getKLabel(atts)) :: downPrioritiesHelp(rest)
    case _ => List.empty
  }

  def downPriorities(parsedProductions: K): Set[Sentence] = parsedProductions match {
    case KApply(KLabelLookup("KProductionBlocks"), KList(productionBlocks), _) => Set(SyntaxPriority(downPrioritiesHelp(productionBlocks).map(>)))
    case _ => Set.empty
  }

  def downProductions(sort: ADT.SortLookup, parsedProductions: K): Set[Sentence] = parsedProductions match {
    case KApply(KLabelLookup("KProductionBlocks"), KList(productionBlocks), _) => productionBlocks.toSet flatMap ((pb:K) => downProductions(sort, pb))
    case KApply(KLabelLookup("KProductionBlock"), KList(productions), _) => productions.toSet flatMap ((p:K) => downProductions(sort, p))
    case KApply(KLabelLookup("KProductionWithAttributes"), KList(productionItems :: atts :: _), _) => Set(Production(sort, downProductionItems(productionItems), downAttributes(atts)))
    case _ => Set(Production(sort, downProductionItems(parsedProductions), Att()))
  }

  def downSentences(parsedSentence: K): Set[Sentence] = parsedSentence match {
    case KApply(KLabelLookup("KSentenceList"), KList(sentence :: rest :: _), _) => downSentences(sentence) ++ downSentences(rest)
    case KApply(KLabelLookup("KSortDecl"), KList(KToken(sortName, KSort, _) :: _), _) => Set(SyntaxSort(Sort(sortName), Att()))
    case KApply(KLabelLookup("KSortDeclWithAttributes"), KList(KToken(sortName, KSort, _) :: atts :: _), _) => Set(SyntaxSort(Sort(sortName), downAttributes(atts)))
    case KApply(KLabelLookup("KSyntax"), KList(KToken(sortName, KSort, _) :: prods :: _), _) => downProductions(Sort(sortName), prods) ++ downPriorities(prods)
    case _ => Set.empty
  }

  def downImports(parsedImports: K): List[String] = parsedImports match {
    case KApply(KLabelLookup("KImportList"), KList(importStmt :: rest :: _), _) => downImports(importStmt) ++ downImports(rest)
    case KApply(KLabelLookup("KImport"), KList(KToken(importModule, KModuleName, _) :: _), _) => List(importModule)
    case _ => List.empty
  }

  // TODO: Make this chase the requires list
  def downModules(parsedModule: K, downedModules: Map[String, Module]): Map[String, Module] = parsedModule match {
    case KApply(KLabelLookup("KDefinition"), KList(requires :: modules :: _), _) => downModules(modules, downModules(requires, downedModules))
    case KApply(KLabelLookup("KRequireList"), _, _) => downedModules
    case KApply(KLabelLookup("KModuleList"), KList(module :: modules :: _), _) => downModules(modules, downModules(module, downedModules))
    case KApply(KLabelLookup("KModule"), KList(KToken(name, KModuleName, _) :: imports :: sentences :: _), _)
      => downedModules ++ Map(name -> Module(name, downImports(imports) map downedModules toSet, downSentences(sentences)))
    case _ => downedModules
  }

  def preProcess(parsed: K): K = parsed match {
    case KToken(str, KString, atts) => KToken(StringEscapeUtils.unescapeJava(str.drop(1).dropRight(1)), KString, atts)
    case kt@KToken(_, _, _) => kt
    case KApply(head, KList(subNodes), atts) => KApply(head, KList(subNodes map preProcess), atts)
  }
}
