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
  import KoreDefintion._
  import KoreDefinitionDown._

  val kParser = new ParseInModule(KDEFINITION)
  def parseTest(parser: ParseInModule, toParse: String, parseAs: SortLookup): K =
    parser.parseString(toParse, parseAs, Source(""))._1 match {
      case Right(x) => x
      case Left(y) => throw new Error("parseTest error: " + y.toString)
    }
  def parseK(toParse: String, parseAs: SortLookup): K = parseTest(kParser, toParse, parseAs)

//  def ktokensFixpoint(): Unit = {
//    println(parseK(KTOKENS_STRING, KModule))
//    assertEquals(KTOKENS, downModules(KTOKENS_STRING, Map())("KTOKENS"))
//  }
//
//  def kmlFixpoint(): Unit = {
//    assertEquals(KML, downModules(KML_STRING, Map("KTOKENS" -> KTOKENS))("KML"))
//  }
//
//  def kattributesFixpoint(): Unit = {
//    assertEquals(KATTRIBUTES, downModules(KATTRIBUTES_STRING, Map("KTOKENS" -> KTOKENS))("KATTRIBUTES"))
//  }
//
//  def ksentencesFixpoint(): Unit = {
//    assertEquals(KSENTENCES, downModules(KSENTENCES_STRING, Map("KTOKENS" -> KTOKENS, "KATTRIBUTES" -> KATTRIBUTES, "KML" -> KML))("KSENTENCES"))
//  }
//
//  def kdefinitionFixpoint(): Unit = {
//    assertEquals(KDEFINITION, downModules(KDEFINITION_STRING, Map("KTOKENS" -> KTOKENS, "KATTRIBUTES" -> KATTRIBUTES, "KML" -> KML, "KSENTENCES" -> KSENTENCES))("KDEFINITION"))
//  }
//
//  def entireDefinitionFixpoint(): Unit = {
//    assertEquals(KOREDEF, downModules(parseK(KORE_STRING, KDefinition), Map("KTOKENS" -> KTOKENS)))
//  }
//
//  def actualFixpoint(): Unit = {
//    val KDEF_PARSED_DOWN = downModules(parseK(KORE_STRING, KDefinition), Map("KTOKENS" -> KTOKENS))("KDEFINITION")
//    val newKParser = new ParseInModule(KDEF_PARSED_DOWN)
//    assertEquals(KOREDEF, downModules(parseTest(newKParser, KORE_STRING, KDefinition), Map("KTOKENS" -> KTOKENS)))
//  }

  @Test def sandboxTest(): Unit = {
    val origString = KORE_STRING
    val downedActual = KORE
    val builtins: Map[String, Module] = Map.empty

    val parsed = parseK(origString, KDefinition)
    println("parsed:")
    println(parsed)
    val downed = downModules(parsed, builtins)
    println("downed:")
    println(downed)
  }

}
