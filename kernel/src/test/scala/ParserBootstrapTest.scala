package org.kframework.definition

import org.kframework.attributes.Source
import org.kframework.parser.concrete2kore.ParseInModule
import org.junit.Test
import org.junit.Assert._
import org.kframework.kore.ADT.SortLookup
import org.kframework.kore._
import org.kframework.kore.KORE._
import org.kframework.builtin.KLabels.ML_FALSE


class KParserBootstrapTest {
  import KParserBootstrap._

  val expParser = new ParseInModule(EXP)
  val kParser = new ParseInModule(KDEFINITION)

  case class l(s: String) { def apply(args: K*): K = KApply(KLabel(s), args) }
  def k(s: String): K = KApply(KLabel(s))
  def t(st: String, sl: ADT.SortLookup): K = KToken(st, sl)

  def parseTest(parser: ParseInModule, toParse: String, parseAs: SortLookup): K =
    parser.parseString(toParse, parseAs, Source(""))._1 match {
      case Right(x) => x
      case Left(y) => k(ML_FALSE)
    }

  def parseK(toParse: String, parseAs: SortLookup): K = parseTest(kParser, toParse, parseAs)

  def getDownedModule(toParse: String, name: String, builtins: Map[String, Module]): Module =
    getAllDownModules(parseK(".KRequireList" + "\n" + toParse + "\n" + ".KModuleList", KDefinition), builtins)(name)

  @Test def simpExp(): Unit = {
    assertEquals(parseTest(expParser, "0 + 0", Exp), l("_+_")(k("0"), k("0")))
  }

  @Test def ktokens(): Unit = {
    assertEquals(parseK("\"aName0239ntehu\"", KString), t("\"aName0239ntehu\"", KString))
    assertEquals(parseK("SortName", KSort), t("SortName", KSort))
    assertEquals(parseK("klabel", KAttributeKey), t("klabel", KAttributeKey))
    assertEquals(parseK("MYMODULE", KModuleName), t("MYMODULE", KModuleName))
  }

  @Test def kml(): Unit = {
    val testVar = l("kmlvar")(t("\"testVar\"", KString))
    val kmlTrue = k("KMLtrue")
    val kmlFalse = k("KMLfalse")
    assertEquals(parseK("kmlvar(\"testVar\")", KMLVar), testVar)
    assertEquals(parseK("KMLtrue", KMLFormula), k("KMLtrue"))
    assertEquals(parseK("KMLfalse", KMLFormula), k("KMLfalse"))
    assertEquals(parseK("kmlvar(\"testVar\") KMLand KMLtrue", KMLFormula), l("KMLand")(testVar, kmlTrue))
    assertEquals(parseK("kmlvar(\"testVar\") KMLor KMLfalse", KMLFormula), l("KMLor")(testVar, kmlFalse))
    assertEquals(parseK("KMLnot kmlvar(\"testVar\")", KMLFormula), l("KMLnot")(testVar))
    assertEquals(parseK("KMLexists kmlvar(\"testVar\") . KMLtrue", KMLFormula), l("KMLexists")(testVar, kmlTrue))
    assertEquals(parseK("KMLforall kmlvar(\"testVar\") . KMLtrue", KMLFormula), l("KMLforall")(testVar, kmlTrue))
    assertEquals(parseK("kmlvar(\"testVar\") KML=> KMLtrue", KMLFormula), l("KMLnext")(testVar, kmlTrue))
  }

  @Test def allDefsTest(): Unit = {
    val parseResult = parseK(ALL_DEFS_STRING, KDefinition)
    //println(parseResult)
    //println(getSortMap(parseResult))
    //println(getASTModules(parseResult))
    assertEquals(getASTModules(parseResult).size, 4)
    assertEquals(getASTModules(parseResult), Set(KML_STRING, KATTRIBUTES_STRING, KSENTENCES_STRING, KDEFINITION_STRING).map(x => parseK(x, KModule)))
  }

  def kattributesFixpoint(): Unit = {
    assertEquals(getDownedModule(KATTRIBUTES_STRING, "KATTRIBUTES", Map("KTOKENS" -> KTOKENS)), KATTRIBUTES)
  }

  def kmlFixpoint(): Unit = {
    assertEquals(getDownedModule(KML_STRING, "KML", Map("KTOKENS" -> KTOKENS, "KATTRIBUTES" -> KATTRIBUTES)), KML)
  }

  def ksentencesFixpoint(): Unit = {
    assertEquals(getDownedModule(KSENTENCES_STRING, "KSENTENCES", Map("KTOKENS" -> KTOKENS, "KATTRIBUTES" -> KATTRIBUTES, "KML" -> KML)), KSENTENCES)
  }

  def kdefinitionFixpoint(): Unit = {
    assertEquals(getDownedModule(KDEFINITION_STRING, "KDEFINITION", Map("KTOKENS" -> KTOKENS, "KATTRIBUTES" -> KATTRIBUTES, "KML" -> KML, "KSENTENCES" -> KSENTENCES)), KDEFINITION)
  }

  @Test def entireDefinitionFixpoint(): Unit = {
    assertEquals(getAllDownModules(parseK(ALL_DEFS_STRING, KDefinition), Map("KTOKENS" -> KTOKENS)),
      Map("KTOKENS" -> KTOKENS, "KATTRIBUTES" -> KATTRIBUTES, "KML" -> KML, "KSENTENCES" -> KSENTENCES, "KDEFINITION" -> KDEFINITION))
  }

  @Test def actualFixpoint(): Unit = {
    val KDEF_PARSED_DOWN = getAllDownModules(parseK(ALL_DEFS_STRING, KDefinition), Map("KTOKENS" -> KTOKENS))("KDEFINITION")
    val newKParser = new ParseInModule(KDEF_PARSED_DOWN)
    assertEquals(getAllDownModules(parseTest(newKParser, ALL_DEFS_STRING, KDefinition), Map("KTOKENS" -> KTOKENS)),
      Map("KTOKENS" -> KTOKENS, "KATTRIBUTES" -> KATTRIBUTES, "KML" -> KML, "KSENTENCES" -> KSENTENCES, "KDEFINITION" -> KDEFINITION))
  }

  @Test def simpleExpModule(): Unit = {
    val MYEXP_STRING =
      """
      module MYEXP
        .KImportList
        syntax MyExp ::= MyExp "*" MyExp   [klabel(kkeyListNil, .KKeyList), left, strict, .KAttributes]
        syntax MyExp ::= MyExp "/" MyExp   [div, left, strict, .KAttributes]
        syntax MyExp ::= MyExp "+" MyExp   [plus, left, strict, .KAttributes]
        .KSentenceList
      endmodule
      """
//    val MYEXP_STRING =
//      """
//      module MYEXP
//        .KImportList
//        .KSentenceList
//      endmodule
//      """

    val MY_K_DEF = ".KRequireList" + MYEXP_STRING + "\n" + ".KModuleList"

    val res = l("module___endmodule")(t("MYEXP",KModuleName)
      , l("__")(l("imports_")(t("BASIC-EXP-SYNTAX", KModuleName)), k(".KImportList"))
      , l("__")(l("_[_]")(l("syntax_::=_")(t("MyExp", KSort), l("__")(t("MyExp",KSort),l("__")(t("\"*\"",KString), t("MyExp",KSort)))),
          l("_,_")(t("mul",KAttributeKey),l("_,_")(t("left",KAttributeKey),l("_,_")(t("strict",KAttributeKey),k(".KAttributes"))))),
        l("__")(l("_[_]")(l("syntax_::=_")(t("MyExp", KSort), l("__")(t("MyExp",KSort),l("__")(t("\"/\"",KString), t("MyExp",KSort)))),
          l("_,_")(t("div",KAttributeKey),l("_,_")(t("left",KAttributeKey),l("_,_")(t("strict",KAttributeKey),k(".KAttributes"))))),
        l("__")(l("_[_]")(l("syntax_::=_")(t("MyExp", KSort), l("__")(t("MyExp",KSort),l("__")(t("\"+\"",KString), t("MyExp",KSort)))),
          l("_,_")(t("plus",KAttributeKey),l("_,_")(t("left",KAttributeKey),l("_,_")(t("strict",KAttributeKey),k(".KAttributes"))))), k(".KSentenceList")))))

    val MyExp = Sort("MyExp")
    val BASICEXPSYNTAX = Module("BASIC-EXP-SYNTAX", imports(), sentences())

    val MYEXP = Module("MYEXP", imports(), sentences(
      syntax(MyExp) is (MyExp, "*", MyExp) att(klabel("mul"), ktoken("left"), ktoken("strict")),
      syntax(MyExp) is (MyExp, "/", MyExp) att(klabel("div"), ktoken("left"), ktoken("strict")),
      syntax(MyExp) is (MyExp, "*", MyExp) att(klabel("plus"), ktoken("left"), ktoken("strict"))
    ))

    //assertEquals(parseK(MYEXP_STRING, KModule), res)

    //println(getSortMap(parseK(MYEXP_STRING, KModule)))
    //println(getSortMap(parseK(KML_STRING, KModule)))

    //val parseResult = parseK(MY_K_DEF, KDefinition)
    //println(getASTNodes(parseResult, "syntax_::=_[_]"))
    //println(parseResult)
    //println(getASTModules(parseResult))
    //println(getAllDownModules(parseResult))

//    val MY_ATT_DEF = ".KRequireList" + KATTRIBUTES_STRING + "\n" + ".KModuleList"
//
//    val parseResult = parseK(MY_ATT_DEF, KDefinition)
//    println(parseResult)
//    println(KATTRIBUTES)
//    println(getAllDownModules(parseResult, Map("KTOKENS" -> KTOKENS)))

  }

}
