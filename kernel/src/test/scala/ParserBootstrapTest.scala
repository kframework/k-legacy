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
  import ExpDefinition._

  val kParser = new ParseInModule(KDEFINITION)
  def runParser(parser: ParseInModule, toParse: String, parseAs: SortLookup): K =
    parser.parseString(toParse, parseAs, Source(""))._1 match {
      case Right(x) => x
      case Left(y) => throw new Error("runParser error: " + y.toString)
    }
  def parseK(toParse: String, parseAs: SortLookup): K = runParser(kParser, toParse, parseAs)

  def helper(): Unit = {
    val expParser = new ParseInModule(EXP)
    println(runParser(expParser, "a + b * c", Exp))
  }

  def kdefFixpoint(): Unit = {

    val KORE_STRING = io.Source.fromFile("src/test/scala/kore.k").mkString

    val builtins = Map("KSTRING" -> KSTRING)

    println(KSENTENCES)

    val parsed = parseK(KORE_STRING, KDefinition)
    val downed = downModules(parsed, builtins)

    println("DOWNED:")
    println("=======")
    println(downed)
    println("=======")

    assertEquals(KSORT, downed("KSORT"))
    assertEquals(KBASIC, downed("KBASIC"))
    assertEquals(KATTRIBUTES, downed("KATTRIBUTES"))
    assertEquals(KSTRING, downed("KSTRING"))
    assertEquals(KML, downed("KML"))
    assertEquals(KSENTENCES, downed("KSENTENCES"))
    assertEquals(KDEFINITION, downed("KDEFINITION"))
  }
}
