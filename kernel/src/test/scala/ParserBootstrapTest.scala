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
  import KParserBootstrapDSL._
  import KParserBootstrapDown._

  val expParser = new ParseInModule(EXP)
  val kParser = new ParseInModule(KDEFINITION)

  //case class l(s: String) { def apply(args: K*): K = KApply(KLabel(s), args) }
  //def k(s: String): K = KApply(KLabel(s))
  def t(st: String, sl: ADT.SortLookup): K = KToken(st, sl)

  def parseTest(parser: ParseInModule, toParse: String, parseAs: SortLookup): K =
    parser.parseString(toParse, parseAs, Source(""))._1 match {
      case Right(x) => x
      case Left(y) => throw new Error("parseTest error: " + y.toString)
    }

  def parseK(toParse: String, parseAs: SortLookup): K = parseTest(kParser, toParse, parseAs)

  def getDownedModule(toParse: String, name: String, builtins: Map[String, Module]): Module =
    getAllDownModules(parseK(toParse, KDefinition), builtins)(name)

  def ktokens(): Unit = {
    println(parseK("r\"sdlakfj\" \"adslkfj\"", KProduction))
    assertEquals(parseK("syntax Test ::= " + rString(KRegexString2) + " [.KAttributes]", KSentence), t("\"aName0239ntehu\"", KString))
//    assertEquals(parseK("SortName", KSort), t("SortName", KSort))
//    assertEquals(parseK("klabel", KAttributeKey), t("klabel", KAttributeKey))
//    assertEquals(parseK("MYMODULE", KModuleName), t("MYMODULE", KModuleName))
  }

  def allDefsTest(): Unit = {
    val parseResult = parseK(ALL_DEFS_STRING, KDefinition)
    //println(parseResult)
    //println(getSortMap(parseResult))
    //println(getASTModules(parseResult))
    assertEquals(getASTModules(parseResult).size, 4)
    assertEquals(getASTModules(parseResult), Set(KML_STRING, KATTRIBUTES_STRING, KSENTENCES_STRING, KDEFINITION_STRING).map(x => parseK(x, KModule)))
  }

  def ktokensFixpoint(): Unit = {
    println(parseK(KTOKENS_STRING, KModule))
    assertEquals(getDownedModule(KTOKENS_STRING, "KTOKENS", Map()), KTOKENS)
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
    //println(testDown(parseK(ALL_DEFS_STRING, KDefinition)))
    assertEquals(getAllDownModules(parseK(ALL_DEFS_STRING, KDefinition), Map("KTOKENS" -> KTOKENS)),
      Map("KTOKENS" -> KTOKENS, "KATTRIBUTES" -> KATTRIBUTES, "KML" -> KML, "KSENTENCES" -> KSENTENCES, "KDEFINITION" -> KDEFINITION))
  }

  @Test def actualFixpoint(): Unit = {
    val KDEF_PARSED_DOWN = getAllDownModules(parseK(ALL_DEFS_STRING, KDefinition), Map("KTOKENS" -> KTOKENS))("KDEFINITION")
    val newKParser = new ParseInModule(KDEF_PARSED_DOWN)
    assertEquals(getAllDownModules(parseTest(newKParser, ALL_DEFS_STRING, KDefinition), Map("KTOKENS" -> KTOKENS)),
      Map("KTOKENS" -> KTOKENS, "KATTRIBUTES" -> KATTRIBUTES, "KML" -> KML, "KSENTENCES" -> KSENTENCES, "KDEFINITION" -> KDEFINITION))
  }

  @Test def sandboxTest(): Unit = {
    val parsed = parseK(KORE_STRING, KDefinition)
    println(parsed)
    val downed = getAllDownModules(parsed)
    assertEquals(downed, Map("KSORT" -> KSORT, "KBASIC" -> KBASIC, "KSTRING" -> KSTRING))
  }

}
