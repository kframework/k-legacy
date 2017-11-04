package org.kframework.definition

import org.kframework.attributes.Att
import org.kframework.builtin.Sorts
import org.kframework.kore.ADT._
import org.kframework.kore._

import collection.JavaConverters._
//import collection.immutable.Set._

/**
  * Created by lpena on 10/11/16.
  */

object KParserBootstrap {

  implicit def BecomingNonTerminal(s: ADT.SortLookup): NonTerminal = NonTerminal(s)
  implicit def BecomingTerminal(s: String): Terminal = Terminal(s)
  implicit def BecomingSequence(ps: ProductionItem*): Seq[ProductionItem] = ps

  //implicit def BecomingAtt(s: String): K =

  def Sort(s: String): ADT.SortLookup = ADT.SortLookup(s)

  def regex(s: String): ProductionItem = RegexTerminal("#", s, "#")

  case class token(s: ADT.SortLookup) {
    def is(pis: ProductionItem): BecomingToken = BecomingToken(s, List(pis))
  }

  case class BecomingToken(sort: ADT.SortLookup, pis: Seq[ProductionItem]) {
    def att(atts: K*): Production = Production(sort, pis, atts.foldLeft(Att() + "token")(_+_))
  }

  implicit def tokenWithoutAttributes(bp: BecomingToken) : Production =
    Production(bp.sort, bp.pis, Att() + "token")

  case class syntax(s: ADT.SortLookup) {
    def is(pis: ProductionItem*): BecomingSyntax = BecomingSyntax(s, pis)
  }

  case class BecomingSyntax(sort: ADT.SortLookup, pis: Seq[ProductionItem]) {
    def att(atts: K*): Production = Production(sort, pis, atts.foldLeft(Att())(_+_))
  }

  implicit def syntaxWithoutAttributes(bp: BecomingSyntax) : Production =
    Production(bp.sort, bp.pis, Att())

  case class Axiom(ax: String, attr: Att) extends Sentence {
    val att = attr
  }

  def axiom(ax: String): BecomingAxiom = BecomingAxiom(ax)

  case class BecomingAxiom(ax: String) {
    def att(atts: K*): Axiom = Axiom(ax, atts.foldLeft(Att())(_+_))
  }

  implicit def axiomWithoutAttributes(bax: BecomingAxiom) : Axiom =
    Axiom(bax.ax, Att())

  def asKList(label: String, values: List[String]): K =
    KORE.KApply(KORE.KLabel(label), KORE.KList(values map { value => KORE.KToken(value, Sorts.KString, Att()) }), Att())

  def imports(s: Module*): Set[Module] = s.toSet
  def sentences(s: Sentence*): Set[Sentence] = s.toSet
  def klabel(label: String): K = asKList("klabel", List(label))
  def ktoken(label: String): K = Att.asK("ktoken", label)

  // TODO: Suggestion: Change `getASTNodes` to `byLabel`
  def getASTNodes(parsed: K, nodeLabel: String): List[K] = parsed match {
    case node@KApply(nl, klist, _) => klist.items.asScala.flatMap(x => getASTNodes(x, nodeLabel)).toList ++ (if (nl == KLabelLookup(nodeLabel)) List(node) else List.empty)
    case _ => List.empty
  }
  def getASTNodes(parsed: K, nodeLabels: String*): List[K] = nodeLabels.foldLeft(List.empty: List[K]) ((acc, nL) => acc ++ getASTNodes(parsed, nL))

  def getASTTokens(parsed: K, tokenName: ADT.SortLookup): List[K] = parsed match {
    case KApply(_, KList(rest), _) => rest flatMap (x => getASTTokens(x, tokenName))
    case token@KToken(_, tn, _) if tokenName == tn => List(token)
    case _ => List.empty
  }

  def getASTModules(parsed: K): Set[K] = getASTNodes(parsed, "KModule") toSet

  def decomposeModule(parsedModule: K): (String, Set[String], Set[K]) = parsedModule match {
    case module@KApply(KLabelLookup("KModule"), KList(KToken(name, _, _) :: importList :: _), _) =>
      (name, getASTNodes(importList, "KImport") map { case KApply(_, KList(KToken(mn, _, _) :: _), _) => mn } toSet, getASTNodes(parsedModule, "KSentenceSyntax") toSet)
  }

  def downAttribute(attr: K): K = attr match {
    case KApply(KLabelLookup("KAttributeApply"), KList(KToken(fnc, KAttributeKey, _) :: keyList :: _), _)
    => asKList(fnc, getASTTokens(keyList, KAttributeKey) map { case KToken(arg, KAttributeKey, _) => arg })
    case _ => attr
  }

  def downAttributes(parsedAttributes: K): Att =
    getASTNodes(parsedAttributes, "KAttribute", "KAttributeApply").foldLeft(Att()) ((acc, curr) => acc.add(downAttribute(curr)))

  def productionSeq(prod: K): Seq[ProductionItem] = prod match {
    case KApply(KLabelLookup("KProduction"), KList(KToken(str, KString, _) :: rest :: _), _) => Terminal(str.drop(1).dropRight(1)) +: productionSeq(rest)
    case KToken(str, KString, _) => Seq(Terminal(str.drop(1).dropRight(1)))
    case KApply(KLabelLookup("KProduction"), KList(KToken(sortName, KSort, _) :: rest :: _), _) => NonTerminal(Sort(sortName)) +: productionSeq(rest)
    case KToken(sortName, KSort, _) => Seq(NonTerminal(Sort(sortName)))
    case KApply(KLabelLookup("KProduction"), KList(KApply(KLabelLookup("KRegex"), KList(KToken(str, KString, _) :: _), _) :: rest :: _), _) => Terminal(str.drop(1).dropRight(1)) +: productionSeq(rest)
  }

  def downSentence(parsedSentence: K): Production = parsedSentence match {
    case KApply(KLabelLookup("KSentenceSyntax"), KList(KToken(sortName, _, _) :: production :: atts :: _), _) =>
      Production(Sort(sortName), productionSeq(production), downAttributes(atts))
  }

  def getAllDownModules(parsed: K, builtins: Map[String, Module]): Map[String, Module] = {
    var decomposedModules = getASTNodes(parsed, "KModule") map decomposeModule
    var downedModules = builtins

    while (decomposedModules.nonEmpty) {
      val (moduleName, importSet, sentences) = decomposedModules.find { case (_, importSet, _) => importSet subsetOf downedModules.keySet }.get
      decomposedModules = decomposedModules.filterNot { case (mn, _, _) => mn == moduleName }
      downedModules += moduleName -> Module(moduleName, importSet map downedModules, sentences map downSentence)
    }
    downedModules
  }

  def getAllDownModules(parsed: K): Map[String, Module] = getAllDownModules(parsed, Map.empty: Map[String, Module])

  val KRegexString = "[\\\"][\\.A-Za-z\\-0-9]*[\\\"]"
//  val KRegexString = "[\\\"]( ([^\\\"\n\r\\\\])" +
//    "                       | ([\\\\][nrtf\\\"\\\\])" +
//    "                       | ([\\\\][x][0-9a-fA-F]{2})" +
//    "                       | ([\\\\][u][0-9a-fA-F]{4})" +
//    "                       | ([\\\\][U][0-9a-fA-F]{8})" +
//    "                       )*" +
//    "                 [\\\"]"
  val KRegexSort = "[A-Z][A-Za-z0-9]*"
  val KRegexAttributeKey = "[\\.A-Za-z\\-0-9]*"
  val KRegexModuleName = "[A-Z][A-Z\\-]*"
  def rString(str: String) : String = "r\"" + str + "\""

  val KTOKENS_STRING =
    """
    module KTOKENS
      .KImportList

      token KString ::= """ + rString(KRegexString) + """ [klabel(KString, .KKeyList), .KAttributes]
      token KSort ::= """ + rString(KRegexSort) + """ [klabel(KSort, .KKeyList), .KAttributes]
      token KAttributeKey ::= """ + rString(KRegexAttributeKey) + """ [klabel(KAttributeKey, .KKeyList), .KAttributes]
      token KModuleName ::= """ + rString(KRegexModuleName) + """ [klabel(KModuleName, .KKeyList), .KAttributes]
      .KSentenceList
    endmodule
    """

  val KString = Sort("KString")
  val KSort = Sort("KSort")
  val KAttributeKey = Sort("KAttributeKey")
  val KModuleName = Sort("KModuleName")

  val KTOKENS = Module("KTOKENS", imports(), sentences(

    token(KString) is regex(KRegexString) att klabel("KString"),
    token(KSort) is regex(KRegexSort) att klabel("KSort"),
    token(KAttributeKey) is regex(KRegexAttributeKey) att klabel("KAttributeKey"),
    token(KModuleName) is regex(KRegexModuleName) att klabel("KModuleName")

  ))

  val KML_STRING =
    """
    module KML
      imports KTOKENS .KImportList
      syntax KMLVar ::= "kmlvar" "(" KString ")" [klabel(kmlvar, .KKeyList), .KAttributes]
      syntax KMLFormula ::= KMLVar [.KAttributes]
      syntax KMLFormula ::= "KMLtrue" [klabel(KMLtrue, .KKeyList), .KAttributes]
      syntax KMLFormula ::= "KMLfalse" [klabel(KMLfalse, .KKeyList), .KAttributes]
      syntax KMLFormula ::= KMLFormula "KMLand" KMLFormula [klabel(KMLand, .KKeyList), .KAttributes]
      syntax KMLFormula ::= KMLFormula "KMLor" KMLFormula [klabel(KMLor, .KKeyList), .KAttributes]
      syntax KMLFormula ::= "KMLnot" KMLFormula [klabel(KMLnot, .KKeyList), .KAttributes]
      syntax KMLFormula ::= "KMLexists" KMLVar "." KMLFormula [klabel(KMLexists, .KKeyList), .KAttributes]
      syntax KMLFormula ::= "KMLforall" KMLVar "." KMLFormula [klabel(KMLforall, .KKeyList), .KAttributes]
      syntax KMLFormula ::= KMLFormula "KML=>" KMLFormula [klabel(KMLnext, .KKeyList), .KAttributes]
      .KSentenceList
    endmodule
    """

  val KMLVar = Sort("KMLVar")
  val KMLFormula = Sort("KMLFormula")

  val KML = Module("KML", imports(KTOKENS), sentences(

    syntax(KMLVar) is ("kmlvar", "(", KString, ")") att klabel("kmlvar"),

    syntax(KMLFormula) is KMLVar,
    syntax(KMLFormula) is "KMLtrue" att klabel("KMLtrue"),
    syntax(KMLFormula) is "KMLfalse" att klabel("KMLfalse"),
    syntax(KMLFormula) is (KMLFormula, "KMLand", KMLFormula) att klabel("KMLand"),
    syntax(KMLFormula) is (KMLFormula, "KMLor", KMLFormula) att klabel("KMLor"),
    syntax(KMLFormula) is ("KMLnot", KMLFormula) att klabel("KMLnot"),
    syntax(KMLFormula) is ("KMLexists", KMLVar, ".", KMLFormula) att klabel("KMLexists"),
    syntax(KMLFormula) is ("KMLforall", KMLVar, ".", KMLFormula) att klabel("KMLforall"),
    syntax(KMLFormula) is (KMLFormula, "KML=>", KMLFormula) att klabel("KMLnext")

  ))

  val KATTRIBUTES_STRING =
    """
    module KATTRIBUTES
      imports KTOKENS .KImportList
      syntax KKeyList ::= ".KKeyList" [klabel(.KKeyList, .KKeyList), .KAttributes]
      syntax KKeyList ::= KAttributeKey "," KKeyList [klabel(KKeyList, .KKeyList), .KAttributes]
      syntax KAttribute ::= KAttributeKey [.KAttributes]
      syntax KAttribute ::= KAttributeKey "(" KKeyList ")" [klabel(KAttributeApply, .KKeyList), .KAttributes]
      syntax KAttributes ::= ".KAttributes" [klabel(.KAttributes, .KKeyList), .KAttributes]
      syntax KAttributes ::= KAttribute "," KAttributes [klabel(KAttributes, .KKeyList), .KAttributes]
      .KSentenceList
    endmodule
    """

  val KKeyList = Sort("KKeyList")
  val KAttribute = Sort("KAttribute")
  val KAttributes = Sort("KAttributes")

  val KATTRIBUTES = Module("KATTRIBUTES", imports(KTOKENS), sentences(

    syntax(KKeyList) is ".KKeyList" att klabel(".KKeyList"),
    syntax(KKeyList) is (KAttributeKey, ",", KKeyList) att klabel("KKeyList"),


    syntax(KAttribute) is KAttributeKey,
    syntax(KAttribute) is (KAttributeKey, "(", KKeyList, ")") att klabel("KAttributeApply"),
    syntax(KAttributes) is ".KAttributes" att klabel(".KAttributes"),
    syntax(KAttributes) is (KAttribute, ",", KAttributes) att klabel("KAttributes")

  ))

  val KSENTENCES_STRING =
    """
    module KSENTENCES
      imports KATTRIBUTES
      imports KML
      .KImportList
      syntax KImport ::= "imports" KModuleName [klabel(KImport, .KKeyList), .KAttributes]
      syntax KImportList ::= ".KImportList" [klabel(.KImportList, .KKeyList), .KAttributes]
      syntax KImportList ::= KImport KImportList [klabel(KImportList, .KKeyList), .KAttributes]
      syntax KTerminal ::= KString [.KAttributes]
      syntax KTerminal ::= "r" KString [klabel(KRegex, .KKeyList), .KAttributes]
      syntax KNonTerminal ::= KSort [.KAttributes]
      syntax KProductionItem ::= KTerminal [.KAttributes]
      syntax KProductionItem ::= KNonTerminal [.KAttributes]
      syntax KProduction ::= KProductionItem [.KAttributes]
      syntax KProduction ::= KProductionItem KProduction [klabel(KProduction, .KKeyList), .KAttributes]
      syntax KSentence ::= "syntax" KSort "::=" KProduction "[" KAttributes "]" [klabel(KSentenceSyntax, .KKeyList), .KAttributes]
      syntax KSentenceList ::= ".KSentenceList" [klabel(.KSentenceList, .KKeyList), .KAttributes]
      syntax KSentenceList ::= KSentence KSentenceList [klabel(KSentenceList, .KKeyList), .KAttributes]
      .KSentenceList
    endmodule
    """


  val KImport = Sort("KImport")
  val KImportList = Sort("KImportList")

  val KTerminal = Sort("KTerminal")
  val KNonTerminal = Sort("KNonTerminal")
  val KProductionItem = Sort("KProductionItem")
  val KProduction = Sort("KProduction")

  val KSentence = Sort("KSentence")
  val KSentenceList = Sort("KSentenceList")

  val KSENTENCES = Module("KSENTENCES", imports(KATTRIBUTES, KML), sentences(

    syntax(KImport) is ("imports", KModuleName) att klabel("KImport"),
    syntax(KImportList) is ".KImportList" att klabel(".KImportList"),
    syntax(KImportList) is (KImport, KImportList) att klabel("KImportList"),

    syntax(KTerminal) is KString,
    syntax(KTerminal) is ("r", KString) att klabel("KRegex"),
    syntax(KNonTerminal) is KSort,
    syntax(KProductionItem) is KTerminal,
    syntax(KProductionItem) is KNonTerminal,
    syntax(KProduction) is KProductionItem,
    syntax(KProduction) is (KProductionItem, KProduction) att klabel("KProduction"),
    syntax(KSentence) is ("syntax", KSort, "::=", KProduction, "[", KAttributes, "]") att klabel("KSentenceSyntax"),

    syntax(KSentenceList) is ".KSentenceList" att klabel(".KSentenceList"),
    syntax(KSentenceList) is (KSentence, KSentenceList) att klabel("KSentenceList")

  ))

  val KDEFINITION_STRING =
    """
    module KDEFINITION
      imports KSENTENCES .KImportList
      syntax KModule ::= "module" KModuleName KImportList KSentenceList "endmodule" [klabel(KModule, .KKeyList), .KAttributes]
      syntax KModuleList ::= ".KModuleList" [klabel(.KModuleList, .KKeyList), .KAttributes]
      syntax KModuleList ::= KModule KModuleList [klabel(KModuleList, .KKeyList), .KAttributes]
      syntax KRequire ::= "require" KString [klabel(KRequire, .KKeyList), .KAttributes]
      syntax KRequireList ::= ".KRequireList" [klabel(.KRequireList, .KKeyList), .KAttributes]
      syntax KRequireList ::= KRequire KRequireList [klabel(KRequireList, .KKeyList), .KAttributes]
      syntax KDefinition ::= KRequireList KModuleList [klabel(KDefinition, .KKeyList), .KAttributes]
      .KSentenceList
    endmodule
    """

  val KModule = Sort("KModule")
  val KModuleList = Sort("KModuleList")

  val KRequire = Sort("KRequire")
  val KRequireList = Sort("KRequireList")
  val KDefinition = Sort("KDefinition")

  val KDEFINITION = Module("KDEFINITION", imports(KSENTENCES), sentences(

    syntax(KModule) is ("module", KModuleName, KImportList, KSentenceList, "endmodule") att klabel("KModule"),
    syntax(KModuleList) is ".KModuleList" att klabel(".KModuleList"),
    syntax(KModuleList) is (KModule, KModuleList) att klabel("KModuleList"),

    syntax(KRequire) is ("require", KString) att klabel("KRequire"),
    syntax(KRequireList) is ".KRequireList" att klabel(".KRequireList"),
    syntax(KRequireList) is (KRequire, KRequireList) att klabel("KRequireList"),

    syntax(KDefinition) is (KRequireList, KModuleList) att klabel("KDefinition")

  ))

  val ALL_DEFS_STRING = ".KRequireList" + "\n" + KML_STRING + "\n" + KATTRIBUTES_STRING + "\n" + KSENTENCES_STRING + "\n" + KDEFINITION_STRING + "\n" + ".KModuleList"

  val EXP_STRING =
    """
    module EXP
      .KImportList
      syntax Exp ::= "0" [.KAttributes]
      syntax Exp ::= "1" [.KAttributes]
      syntax Exp ::= Exp "+" Exp [.KAttributes]
      syntax Exp ::= Exp "*" Exp [.KAttributes]
      .KSentenceList
    endmodule
    """

  val Exp = Sort("Exp")

  val EXP = Module("EXP", imports(), sentences(

    syntax(Exp) is "0" att klabel("0"),
    syntax(Exp) is "1" att klabel("1"),
    syntax(Exp) is (Exp, "+", Exp) att klabel("_+_"),
    syntax(Exp) is (Exp, "*", Exp) att klabel("_*_")

  ))

}
