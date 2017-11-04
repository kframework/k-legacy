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
  import KDefinitionDSL._
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

  def regexStringTest(): Unit = {
    println("KRegexString: " + KRegexString)
    println("KRegex: " + regex(KRegexString).toString)
    println(parseK(""""oenth"""", KString))
    println(parseK(""""renuth"""", KString))
  }

  @Test def kdefFixpoint(): Unit = {

    val KORE_STRING = io.Source.fromFile("src/test/scala/kore.k").mkString

    val parsed = preProcess(parseK(KORE_STRING, KDefinition))
    val builtins: Map[String, Module] = Map.empty
    val downed = downModules(parsed, builtins)

    val modules = List( ("KSORT", KSORT)
                      , ("KBASIC", KBASIC)
                      , ("KSTRING", KSTRING)
                      , ("KATTRIBUTES", KATTRIBUTES)
                      , ("KML", KML)
                      , ("KSENTENCES", KSENTENCES)
                      , ("KDEFINITION", KDEFINITION)
                      )

    modules.foreach { case (name, module) => 
      // println(name ++ " ORIG:")
      // println("==============")
      // println(module)
      // println("==============")
      // println(name ++ " DOWNED:")
      // println("================")
      // println(downed(name))
      // println("================")
      // println("\n\n")
      assertEquals(module, downed(name))
    }
  }
}
