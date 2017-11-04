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
}

object ExpDefinition {
  import KDefinitionDSL._

  val Exp = Sort("Exp")
  val EXP = Module("EXP", imports(), sentences(
    syntax(Exp) is (Exp, "+", Exp) att(klabel("p"), "plus"),
    syntax(Exp) is (Exp, "-", Exp) att("minus", klabel("m")),
    syntax(Exp) is (Exp, "*", Exp) att(klabel("t"), "times"),
    syntax(Exp) is (Exp, "/", Exp) att(klabel("d"), "div"),
    priority( >("p", "t") , >("m", "d") )
  ))
}

object KoreDefintion {
  import KDefinitionDSL._


  // ### KSTRING
  val KRegexString = "[\"](([^\n\r\t\f\"\\\\])|([\\\\][nrtf\"\\\\])|([\\\\][x][0-9a-fA-F]{2})|([\\\\][u][0-9a-fA-F]{4})|([\\\\][U][0-9a-fA-F]{8}))*[\"]"

  val KString = Sort("KString")

  val KSTRING = Module("KSTRING", imports(), sentences(
    syntax(KString) is regex(KRegexString) att("token", khook("org.kframework.kore.KString"))
  ))


  // ### KATTRIBUTES
  val KRegexAttributeKey1 = "[\\.A-Za-z\\-0-9]*"
  val KRegexAttributeKey2 = "`(\\\\`|\\\\\\\\|[^`\\\\\n\r\t\f])+`"
  val KRegexAttributeKey3 = "(?![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*"
  // val KRegexAttributeKey3 = """(?<![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*"""
  // the (?<! is a signal to the parser that it should be used as a "precedes" clause, do we need it?

  val KAttributeKey = Sort("KAttributeKey")
  val KKeyList = Sort("KKeyList")
  val KKeySet = Sort("KKeySet")
  val KAttribute= Sort("KAttribute")
  val KAttributes= Sort("KAttributes")

  val KATTRIBUTES = Module("KATTRIBUTES", imports(), sentences(
    syntax(KAttributeKey) is regex(KRegexAttributeKey1) att("token", khook("org.kframework.kore.KLabel")),
    syntax(KAttributeKey) is regex(KRegexAttributeKey2) att("token", khook("org.kframework.kore.KLabel")),
    syntax(KAttributeKey) is regex(KRegexAttributeKey3) att("token", khook("org.kframework.kore.KLabel"), "autoReject"),

    syntax(KKeyList) is KAttributeKey,
    syntax(KKeyList) is "" att klabel(".KKeyList"),
    syntax(KKeyList) is (KKeyList, ",", KKeyList) att(klabel("KKeyList"), "assoc", kunit(".KKeyList")),

    syntax(KKeySet) is KAttributeKey,
    syntax(KKeySet) is "" att klabel(".KKeySet"),
    syntax(KKeySet) is (KKeySet, KKeySet) att(klabel("KKeySet"), "assoc", "comm", kunit(".KKeyList")),

    syntax(KAttribute) is KAttributeKey,
    syntax(KAttribute) is (KAttributeKey, "(", KKeyList, ")") att klabel("KAttributeApply"),

    syntax(KAttributes) is KAttribute,
    syntax(KAttributes) is "" att klabel(".KAttributes"),
    syntax(KAttributes) is (KAttribute, ",", KAttributes) att klabel("KAttributes")
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

  val KPriority = Sort("KPriority")

  val KSentence = Sort("KSentence")
  val KSentenceList = Sort("KSentenceList")

  val KSENTENCES = Module("KSENTENCES", imports(KSTRING, KATTRIBUTES), sentences(
    syntax(KSort) is regex(KRegexSort) att("token", klabel("KSort")),

    syntax(KTerminal) is KString,
    syntax(KTerminal) is ("r", KString) att klabel("KRegex"),
    syntax(KNonTerminal) is KSort,

    syntax(KProduction) is KTerminal,
    syntax(KProduction) is KNonTerminal,
    syntax(KProduction) is (KProduction, KProduction) att(klabel("KProductionItems"), "assoc"),

    syntax(KPriority) is KKeySet,
    syntax(KPriority) is (KPriority, ">", KPriority) att(klabel("KPriorityItems"), "assoc"),

    syntax(KSentence) is ("syntax", KSort) att klabel("KSortDecl"),
    syntax(KSentence) is ("syntax", KSort, "::=", KProduction) att klabel("KProduction"),
    syntax(KSentence) is ("syntax", "priority", KPriority) att klabel("KPriority"),
    syntax(KSentence) is (KSentence, "[", KAttributes, "]") att klabel("KSentenceWithAttributes"),

    syntax(KSentenceList) is KSentence,
    syntax(KSentenceList) is "" att klabel(".KSentenceList"),
    syntax(KSentenceList) is (KSentence, KSentenceList) att(klabel("KSentenceList"))
    // TODO: Why doesn't this work?
    //syntax(KSentenceList) is (KSentenceList, KSentenceList) att(klabel("KSentenceList"), "assoc", "comm", kunit(".KSentenceList"))
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
  val KORE = Map( "KSTRING" -> KSTRING
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
    case KApply(KLabelLookup("KKeyList"), KList(kkeys), _) => kkeys flatMap downKKeyList
    case KToken(att, KAttributeKey, _)                     => List(att)
    case _                                                 => List.empty
  }

  def downKKeySet(parsedKKeySet: K): Set[String] = parsedKKeySet match {
    case KApply(KLabelLookup("KKeySet"), KList(kkeys), _) => kkeys.toSet flatMap downKKeySet
    case KToken(att, KAttributeKey, _)                    => Set(att)
    case _                                                => Set.empty
  }

  def downAttributes(parsedAttributes: K): Att = parsedAttributes match {
    case KApply(KLabelLookup("KAttributes"), KList(atts), _)                                              => atts.foldLeft(Att()) ((accAtt: Att, newAtt: K) => accAtt ++ downAttributes(newAtt))
    case KApply(KLabelLookup("KAttributeApply"), KList(KToken(fnc, KAttributeKey, _) :: keyList :: _), _) => Att(asKApply(fnc, downKKeyList(keyList)))
    case KToken(attName, KAttributeKey, _)                                                                => Att(attName)
    case _                                                                                                => Att()
  }

  def downProduction(parsedProduction: K): Seq[ProductionItem] = parsedProduction match {
    case KApply(KLabelLookup("KProductionItems"), KList(productionItems), _)    => productionItems flatMap downProduction
    case KApply(KLabelLookup("KRegex"), KList(KToken(str, KString, _) :: _), _) => Seq(RegexTerminal("#", str, "#"))
    case KToken(sortName, KSort, _)                                             => Seq(NonTerminal(Sort(sortName)))
    case KToken(str, KString, _)                                                => Seq(Terminal(str))
    case _                                                                      => Seq.empty
  }

  def downPriorityBlocks(parsedPriority: K): Seq[Set[Tag]] = parsedPriority match {
    case KApply(KLabelLookup("KPriorityItems"), KList(priorityBlocks), _) => priorityBlocks flatMap downPriorityBlocks
    case keys@KApply(KLabelLookup("KKeySet"), _, _)                       => Seq(downKKeySet(keys) map Tag)
    case _                                                                => Seq.empty
  }

  def downSentences(parsedSentence: K, atts: Att = Att()): Set[Sentence] = parsedSentence match {
    case KApply(KLabelLookup("KSentenceList"), KList(sentences), _)                                   => sentences.toSet flatMap ((pS: K) => downSentences(pS, Att()))
    case KApply(KLabelLookup("KSentenceWithAttributes"), KList(sentence :: newAtts :: _), _)          => downSentences(sentence, downAttributes(newAtts) ++ atts)
    case KApply(KLabelLookup("KSortDecl"), KList(KToken(sortName, KSort, _) :: _), _)                 => Set(SyntaxSort(Sort(sortName), atts))
    case KApply(KLabelLookup("KProduction"), KList(KToken(sortName, KSort, _) :: production :: _), _) => Set(Production(Sort(sortName), downProduction(production), atts))
    case KApply(KLabelLookup("KPriority"), KList(priority :: _), _)                                   => Set(SyntaxPriority(downPriorityBlocks(priority), atts))
    case _                                                                                            => Set.empty
  }

  def downImports(parsedImports: K): List[String] = parsedImports match {
    case KApply(KLabelLookup("KImportList"), KList(importStmt :: rest :: _), _)               => downImports(importStmt) ++ downImports(rest)
    case KApply(KLabelLookup("KImport"), KList(KToken(importModule, KModuleName, _) :: _), _) => List(importModule)
    case _                                                                                    => List.empty
  }

  // TODO: Make this chase the requires list
  def downModules(parsedModule: K, downedModules: Map[String, Module]): Map[String, Module] = parsedModule match {
    case KApply(KLabelLookup("KDefinition"), KList(requires :: modules :: _), _)                              => downModules(modules, downModules(requires, downedModules))
    case KApply(KLabelLookup("KRequireList"), _, _)                                                           => downedModules
    case KApply(KLabelLookup("KModuleList"), KList(module :: modules :: _), _)                                => downModules(modules, downModules(module, downedModules))
    case KApply(KLabelLookup("KModule"), KList(KToken(name, KModuleName, _) :: imports :: sentences :: _), _) => downedModules ++ Map(name -> Module(name, downImports(imports) map downedModules toSet, downSentences(sentences)))
    case _                                                                                                    => downedModules
  }

  def preProcess(parsed: K): K = parsed match {
    case KToken(str, KString, atts)          => KToken(StringEscapeUtils.unescapeJava(str.drop(1).dropRight(1)), KString, atts)
    case kt@KToken(_, _, _)                  => kt
    case KApply(head, KList(subNodes), atts) => KApply(head, KList(subNodes map preProcess), atts)
  }
}
