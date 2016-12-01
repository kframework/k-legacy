package org.kframework.definition

import org.kframework.attributes.Att
import org.kframework.kore.ADT._
import org.kframework.kore._
import collection.JavaConverters._
//import collection.immutable.Set._

/**
  * Created by lpena on 10/11/16.
  */

object KParserBootsrap {

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

  def imports(s: Module*): Set[Module] = s.toSet
  def sentences(s: Sentence*): Set[Sentence] = s.toSet
  def klabel(label: String): K = Att.asK("klabel", label)
  def ktoken(label: String): K = Att.asK("ktoken", label)

  // TODO: Suggestion: Change `getASTNodes` to `byLabel`
  def getASTNodes(parsed: K, nodeLabel: String): Set[K] = parsed match {
    case node@KApply(nl, klist, _) => klist.items.asScala.flatMap(x => getASTNodes(x, nodeLabel)).toSet ++ (if (nl == KLabelLookup(nodeLabel)) Set(node) else Set.empty)
    case _ => Set.empty
  }
  def getASTNodes(parsed: K, nodeLabels: String*): Set[K] = nodeLabels.foldLeft(Set.empty: Set[K]) ((curr:Set[K], nL:String) => getASTNodes(parsed, nL) ++ curr)

  def getASTModules(parsed: K): Set[K] = getASTNodes(parsed, "module___endmodule")

  def decomposeModule(parsedModule: K): (String, Set[String], Set[K]) = parsedModule match {
    case module@KApply(KLabelLookup("module___endmodule"), KList(KToken(name, _, _) :: importList :: _), _) =>
      (name, getASTNodes(importList, "imports_") map { case KApply(_, KList(KToken(mn, _, _) :: _), _) => mn }, getASTNodes(parsedModule, "syntax_::=_[_]"))
  }

  def productionSeq(prod: K): Seq[ProductionItem] = prod match {
    case KApply(KLabelLookup("__"), KList(KToken(str, KString, _) :: rest :: _), _) => Terminal(str) +: productionSeq(rest)
    case KToken(str, KString, _) => Seq(Terminal(str))
    case KApply(KLabelLookup("__"), KList(KToken(sortName, KSort, _) :: rest :: _), _) => NonTerminal(Sort(sortName)) +: productionSeq(rest)
    case KToken(sortName, KSort, _) => Seq(NonTerminal(Sort(sortName)))
  }

  def downAttribute(attr: K): K = attr match {
    case KApply(KLabelLookup("KAttribute"), KList(KToken(str, KAttributeKey, _) :: _), _) => KString(str)
    case KApply(KLabelLookup("KAttributeApply"), KList(KToken(str, KAttributeKey, _) :: keyList :: _), _) => KApply(KString(str), getASTNodes(keyList, "KAttributeKey").toList, Att())
    case _ => curr
  }

  def downAttributes(parsedAttributes: K): Att =
    getASTNodes(parsedAttributes, "KAttribute", "KAttributeApply").foldLeft(Att()) ((curr, next) -> curr.add(downAttribute(next)))

  def downSentence(parsedSentence: K): Production = parsedSentence match {
    case KApply(KLabelLookup("syntax_::=_[_]"), KList(KToken(sortName, _, _) :: production :: atts :: _), _) =>
      Production(Sort(sortName), productionSeq(production), downAttributes(atts))
  }

  def getAllDownModules(parsed: K): Map[String, Module] = {
    var decomposedModules = getASTNodes(parsed, "module___endmodule") map decomposeModule
    var downedModules = Map.empty : Map[String, Module]

    while (decomposedModules.nonEmpty) {
      val (moduleName, importSet, sentences) = decomposedModules.find { case (_, importSet, _) => importSet subsetOf downedModules.keySet }.get
      decomposedModules = decomposedModules.filterNot { case (mn, _, _) => mn == moduleName }
      downedModules += moduleName -> Module(moduleName, importSet map downedModules, sentences map downSentence)
    }
    downedModules
  }

  val KTOKENS_STRING =
    """
    module KTOKENS
      .KImportList

      token KString       ::= r"\"[a-zA-Z0-9\\-]*\"" [klabel(KString), .Attributes]
      token KSort         ::= r"[A-Z][A-Za-z0-9]*" [klabel(KSort), .Attributes]
      token KAttributeKey ::= r"[a-z][A-Za-z\\-0-9]*" [klabel(KAttributeKey), .Attributes]
      token KModuleName   ::= r"[A-Z][A-Z]*" [klabel(KModuleName), .Attributes]

      .KSentenceList
    endmodule
    """

  val KString = Sort("KString")
  val KSort = Sort("KSort")
  val KAttributeKey = Sort("KAttributeKey")
  val KModuleName = Sort("KModuleName")

  val KTOKENS = Module("KTOKENS", imports(), sentences(

    token(KString) is regex("[\\\"](([^\\\"\n\r\\\\])|([\\\\][nrtf\\\"\\\\])|([\\\\][x][0-9a-fA-F]{2})|([\\\\][u][0-9a-fA-F]{4})|([\\\\][U][0-9a-fA-F]{8}))*[\\\"]") att klabel("KString"),
    token(KSort) is regex("[A-Z][A-Za-z0-9]*") att klabel("KSort"),
    token(KAttributeKey) is regex("[a-z][A-Za-z\\-0-9]*") att klabel("KAttributeKey"),
    token(KModuleName) is regex("[A-Z][A-Z\\-]*") att klabel("KModuleName")

  ))

  val KML_STRING =
    """
    module KML
      imports KTOKENS .KImportList
      syntax KMLVar ::= "kmlvar" "(" KString ")" [.KAttributes]
      syntax KMLFormula ::= KMLVar [.KAttributes]
      syntax KMLFormula ::= "KMLtrue" [.KAttributes]
      syntax KMLFormula ::= "KMLfalse" [.KAttributes]
      syntax KMLFormula ::= KMLFormula "KMLand" KMLFormula [.KAttributes]
      syntax KMLFormula ::= KMLFormula "KMLor" KMLFormula [.KAttributes]
      syntax KMLFormula ::= "KMLnot" KMLFormula [.KAttributes]
      syntax KMLFormula ::= "KMLexists" KMLVar "." KMLFormula [.KAttributes]
      syntax KMLFormula ::= "KMLforall" KMLVar "." KMLFormula [.KAttributes]
      syntax KMLFormula ::= KMLFormula "KML=>" KMLFormula [.KAttributes]
      .KSentenceList
    endmodule
    """

  val KMLVar = Sort("KMLVar")
  val KMLFormula = Sort("KMLFormula")

  val KML = Module("KML", imports(KTOKENS), sentences(

    syntax(KMLVar) is ("kmlvar", "(", KString, ")") att klabel("kmlvar(_)"),

    syntax(KMLFormula) is KMLVar,
    syntax(KMLFormula) is "KMLtrue" att klabel("KMLtrue"),
    syntax(KMLFormula) is "KMLfalse" att klabel("KMLfalse"),
    syntax(KMLFormula) is (KMLFormula, "KMLand", KMLFormula) att klabel("_KMLand_"),
    syntax(KMLFormula) is (KMLFormula, "KMLor", KMLFormula) att klabel("_KMLor_"),
    syntax(KMLFormula) is ("KMLnot", KMLFormula) att klabel("KMLnot_"),
    syntax(KMLFormula) is ("KMLexists", KMLVar, ".", KMLFormula) att klabel("KMLexists_._"),
    syntax(KMLFormula) is ("KMLforall", KMLVar, ".", KMLFormula) att klabel("KMLforall_._"),
    syntax(KMLFormula) is (KMLFormula, "KML=>", KMLFormula) att klabel("_KML=>_")

  ))

  val KATTRIBUTES_STRING =
    """
    module KATTRIBUTES
      imports KTOKENS .KImportList
      syntax KKeyList ::= KAttributeKey [.KAttributes]
      syntax KKeyList ::= KAttributeKey "," KKeyList [.KAttributes]
      syntax KAttribute ::= KAttributeKey [.KAttributes]
      syntax KAttribute ::= KAttributeKey "(" KKeyList ")" [.KAttributes]
      syntax KAttributes ::= ".KAttributes" [.KAttributes]
      syntax KAttributes ::= KAttribute "," KAttributes [.KAttributes]
      .KSentenceList
    endmodule
    """

  val KKeyList = Sort("KeyList")
  val KAttribute = Sort("Attribute")
  val KAttributes = Sort("Attributes")

  val KATTRIBUTES = Module("KATTRIBUTES", imports(KTOKENS), sentences(

    syntax(KKeyList) is KAttributeKey,
    syntax(KKeyList) is (KAttributeKey, ",", KKeyList),

    syntax(KAttribute) is KAttributeKey att klabel("KAttribute"),
    syntax(KAttribute) is (KAttributeKey, "(", KKeyList, ")") att klabel("KAttributeApply"),
    syntax(KAttributes) is ".KAttributes" att klabel(".KAttributes"),
    syntax(KAttributes) is (KAttribute, ",", KAttributes) att klabel("_,_")

  ))

  val KSENTENCES_STRING =
    """
    module KSENTENCES
      imports KATTRIBUTES .KImportList
      syntax KImport ::= "imports" KModuleName [.KAttributes]
      syntax KImportList ::= ".KImportList" [.KAttributes]
      syntax KImportList ::= KImport KImportList [.KAttributes]
      syntax KTerminal ::= KString [.KAttributes]
      syntax KNonTerminal ::= KSort [.KAttributes]
      syntax KProductionItem ::= KTerminal [.KAttributes]
      syntax KProductionItem ::= KNonTerminal [.KAttributes]
      syntax KProduction ::= KProductionItem [.KAttributes]
      syntax KProduction ::= KProductionItem KProduction [.KAttributes]
      syntax KSentence ::= "token" KSort "::=" KProduction "[" KAttributes "]" [.KAttributes]
      syntax KSentence ::= "syntax" KSort "::=" KProduction "[" KAttributes "]" [.KAttributes]
      syntax KSentence ::= "axiom" KMLFormula "[" KAttributes "]" [.KAttributes]
      syntax KSentenceList ::= ".KSentenceList" [.KAttributes]
      syntax KSentenceList ::= KSentence KSentenceList [.KAttributes]
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

    syntax(KImport) is ("imports", KModuleName) att klabel("imports_"),
    syntax(KImportList) is ".KImportList" att klabel(".KImportList"),
    syntax(KImportList) is (KImport, KImportList) att klabel("__"),

    syntax(KTerminal) is KString,
    syntax(KNonTerminal) is KSort,
    syntax(KProductionItem) is KTerminal,
    syntax(KProductionItem) is KNonTerminal,
    syntax(KProduction) is KProductionItem,
    syntax(KProduction) is (KProductionItem, KProduction) att klabel("__"),

    syntax(KSentence) is ("token", KSort, "::=", KProduction, "[", KAttributes, "]") att klabel("token_::=_[_]"),
    syntax(KSentence) is ("syntax", KSort, "::=", KProduction, "[", KAttributes, "]") att klabel("syntax_::=_[_]"),
    syntax(KSentence) is ("axiom", KMLFormula, "[", KAttributes, "]") att klabel("axiom_[_]"),

    syntax(KSentenceList) is ".KSentenceList" att klabel(".KSentenceList"),
    syntax(KSentenceList) is (KSentence, KSentenceList) att klabel("__")

  ))

  val KDEFINITION_STRING =
    """
    module KDEFINITION
      imports KSENTENCES .KImportList
      syntax KModule ::= "module" KModuleName KImportList KSentenceList "endmodule" [.KAttributes]
      syntax KModuleList ::= ".KModuleList" [.KAttributes]
      syntax KModuleList ::= KModule KModuleList [.KAttributes]
      syntax KRequire ::= "require" KString [.KAttributes]
      syntax KRequireList ::= ".KRequireList" [.KAttributes]
      syntax KRequireList ::= Require RequireList [.KAttributes]
      syntax KDefinition ::= KRequireList KModuleList [.KAttributes]
      .KSentenceList
    endmodule
    """

  val KModule = Sort("KModule")
  val KModuleList = Sort("KModuleList")

  val KRequire = Sort("KRequire")
  val KRequireList = Sort("KRequireList")
  val KDefinition = Sort("KDefinition")

  val KDEFINITION = Module("KDEFINITION", imports(KSENTENCES), sentences(

    syntax(KModule) is ("module", KModuleName, KImportList, KSentenceList, "endmodule") att klabel("module___endmodule"),
    syntax(KModuleList) is ".KModuleList" att klabel(".KModuleList"),
    syntax(KModuleList) is (KModule, KModuleList) att klabel("__"),

    syntax(KRequire) is ("require", KString) att klabel("require_"),
    syntax(KRequireList) is ".KRequireList" att klabel(".KRequireList"),
    syntax(KRequireList) is (KRequire, KRequireList) att klabel("__"),

    syntax(KDefinition) is (KRequireList, KModuleList) att klabel("__")

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
