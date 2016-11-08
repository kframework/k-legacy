package org.kframework.definition

import org.kframework.attributes.Att

import org.kframework.kore._

/**
  * Created by lpena on 10/11/16.
  */

object test {

  implicit def BecomingNonTerminal(s: ADT.SortLookup): NonTerminal = NonTerminal(s)
  implicit def BecomingTerminal(s: String): Terminal = Terminal(s)
  implicit def BecomingSequence(ps: ProductionItem*): Seq[ProductionItem] = ps

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

  // module KTOKENS
  //   .KImportList
  //
  //   token KString       ::= r"\"[a-zA-Z0-9\\-]*\"" [klabel(KString), .Attributes]
  //   token KSort         ::= r"[A-Z][A-Za-z0-9]*" [klabel(KSort), .Attributes]
  //   token KAttributeKey ::= r"[a-z][A-Za-z\\-0-9]*" [klabel(KAttributeKey), .Attributes]
  //   token KModuleName   ::= r"[A-Z][A-Z]*" [klabel(KModuleName), .Attributes]
  //
  //   .KSentenceList
  // endmodule

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
    "module KML" +
      "imports KTOKENS .KImportList" +
      "syntax KMLVar ::= \"kmlvar\" \"(\" KString \")\" [klabel(kmlvar(_)), .KAttributes]" +
      "syntax KMLFormula ::= KMLVar [.KAttributes]" +
      "syntax KMLFormula ::= \"KMLtrue\" [klabel(KMLtrue), .KAttributes]" +
      "syntax KMLFormula ::= \"KMLfalse\" [klabel(KMLfalse), .KAttributes]" +
      "syntax KMLFormula ::= KMLFormula \"KMLand\" KMLFormula [klabel(_KMLand_), .KAttributes]" +
      "syntax KMLFormula ::= KMLFormula \"KMLor\" KMLFormula [klabel(_KMLor_), .KAttributes]" +
      "syntax KMLFormula ::= \"KMLnot\" KMLFormula [klabel(KMLnot_), .KAttributes]" +
      "syntax KMLFormula ::= \"KMLexists\" KMLVar \".\" KMLFormula [klabel(KMLexists_._), .KAttributes]" +
      "syntax KMLFormula ::= \"KMLforall\" KMLVar \".\" KMLFormula [klabel(KMLforall_._), .KAttributes]" +
      "syntax KMLFormula ::= KMLFormula \"KML=>\" KMLFormula [klabel(_KML=>_), .KAttributes]" +
      ".KSentenceList" +
    "endmodule"

  val KMLVar = Sort("MLVar")
  val KMLFormula = Sort("MLFormula")

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
    "module KATTRIBUTES" +
      "imports KTOKENS .KImportList" +
      "syntax KKeyList ::= KAttributeKey [.KAttributes]" +
      "syntax KKeyList ::= KAttributeKey "," KKeyList [klabel(_,_), .KAttributes]" +
      "syntax KAttribute ::= KAttributeKey [.Attributes]" +
      "syntax KAttribute ::= KAttributeKey "(" KKeyList ")" [klabel(_(_)), .KAttributes]" +
      "syntax KAttributes ::= \".KAttributes\" [klabel(.KAttributes), .KAttributes]" +
      "syntax KAttributes ::= KAttribute "," KAttributes [klabel(_,_), .KAttributes]" +
      ".KSentenceList" +
    "endmodule"

  val KKeyList = Sort("KeyList")
  val KAttribute = Sort("Attribute")
  val KAttributes = Sort("Attributes")

  val KATTRIBUTES = Module("KATTRIBUTES", imports(KTOKENS), sentences(

    syntax(KKeyList) is KAttributeKey,
    syntax(KKeyList) is (KAttributeKey, ",", KKeyList) att klabel("_,_"),

    syntax(KAttribute) is KAttributeKey,
    syntax(KAttribute) is (KAttributeKey, "(", KKeyList, ")") att klabel("_(_)"),
    syntax(KAttributes) is ".KAttributes" att klabel(".KAttributes"),
    syntax(KAttributes) is (KAttribute, ",", KAttributes) att klabel("_,_")

  ))

  val KSENTENCES_STRING =
    "module KSENTENCES" +
      "imports KATTRIBUTES .KImportList" +
      "syntax KImport = \"imports\" KModuleName [klabel(imports_), .KAttributes]" +
      "syntax KImportList = \".KImportList\" [klabel(.KImportList), .KAttributes]" +
      "syntax KImportList = KImport KImportList [klabel(__), .KAttributes]" +
      "syntax KTerminal ::= KString [.KAttributes]" +
      "syntax KNonTerminal ::= KSort [.KAttributes]" +
      "syntax KProductionItem ::= KTerminal [.KAttributes]" +
      "syntax KProductionItem ::= KNonTerminal [.KAttributes]" +
      "syntax KProduction ::= KProductionItem [.KAttributes]" +
      "syntax KProduction ::= KProductionItem KProduction [klabel(__), .KAttributes]" +
      "syntax KPreSentence = \"token\" KSort \"::=\" KProduction [klabel(token_::=_), .KAttributes]" +
      "syntax KPreSentence = \"syntax\" KSort \"::=\" KProduction [klabel(syntax_::=_), .KAttributes]" +
      "syntax KPreSentence = \"axiom\" KMLFormula [klabel(axiom_), .KAttributes]" +
      "syntax KSentence = KPreStentence \"[\" KAttributes \"]\" [klabel(_[_]), .KAttributes]" +
      "syntax KSentenceList = \".KSentenceList\" [klabel(.KSentenceList), .KAttributes]" +
      "syntax KSentenceList = KSentence KSentenceList [klabel(__), .KAttributes]" +
      ".KSentenceList" +
    "endmodule"

  val KImport = Sort("KImport")
  val KImportList = Sort("KImportList")

  val KTerminal = Sort("KTerminal")
  val KNonTerminal = Sort("KNonTerminal")
  val KProductionItem = Sort("KProductionItem")
  val KProduction = Sort("KProduction")

  val KPreSentence = Sort("KPreSentence")
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

    syntax(KPreSentence) is ("token", KSort, "::=", KProduction) att klabel("token_::=_"),
    syntax(KPreSentence) is ("syntax", KSort, "::=", KProduction) att klabel("syntax_::=_"),
    syntax(KPreSentence) is ("axiom", KMLFormula) att klabel("axiom_"),

    syntax(KSentence) is (KPreSentence, "[", KAttributes, "]") att klabel("_[_]"),
    syntax(KSentenceList) is ".KSentenceList" att klabel(".KSentenceList"),
    syntax(KSentenceList) is (KSentence, KSentenceList) att klabel("__")

  ))

  val KDEFINITION_STRING =
    "module KDEFINITION" +
      "imports KSENTENCES .KImportList" +
      "syntax KModule ::= \"module\" KModuleName KImportList KSentenceList \"endmodule\" [klabel(module___endmodule), .KAttribute]" +
      "syntax KModuleList = KModule [.KAttribute]" +
      "syntax KModuleList = KModule KModuleList [klabel(__), .KAttribute]" +
      "syntax KRequire ::= \"require\" KString [klabel(require_), .KAttribute]" +
      "syntax KRequireList ::= \".KRequireList\" [klabel(.KRequireList), .KAttribute]" +
      "syntax KRequireList ::= Require RequireList [klabel(__), .KAttribute]" +
      "syntax KDefinition ::= KRequireList KModuleList [klabel(__), .KAttribute]" +
      ".KSentenceList" +
    "endmodule"

  val KModule = Sort("KModule")
  val KModuleList = Sort("KModuleList")

  val KRequire = Sort("KRequire")
  val KRequireList = Sort("KRequireList")
  val KDefinition = Sort("KDefinition")

  val KDEFINITION = Module("KDEFINITION", imports(KSENTENCES), sentences(

    syntax(KModule) is ("module", KModuleName, KImportList, KSentenceList, "endmodule") att klabel("module___endmodule"),
    syntax(KModuleList) is KModule,
    syntax(KModuleList) is (KModule, KModuleList) att klabel("__"),

    syntax(KRequire) is ("require", KString) att klabel("require_"),
    syntax(KRequireList) is ".KRequireList" att klabel(".KRequireList"),
    syntax(KRequireList) is (KRequire, KRequireList) att klabel("__"),

    syntax(KDefinition) is (KRequireList, KModuleList) att klabel("__")

  ))

  val EXP_STRING =
    "module EXP" +
      ".KImportList" +
      "syntax Exp ::= \"0\" [klabel(0), .KAttributes]" +
      "syntax Exp ::= \"1\" [klabel(1), .KAttributes]" +
      "syntax Exp ::= Exp \"+\" Exp [klabel(_+_), .KAttributes]" +
      "syntax Exp ::= Exp \"*\" Exp [klabel(_*_), .KAttributes]" +
      ".KSentenceList" +
    "endmodule"

  val Exp = Sort("Exp")

  val EXP = Module("EXP", imports(), sentences(

    syntax(Exp) is "0" att klabel("0"),
    syntax(Exp) is "1" att klabel("1"),
    syntax(Exp) is (Exp, "+", Exp) att klabel("_+_"),
    syntax(Exp) is (Exp, "*", Exp) att klabel("_*_")

  ))

}
