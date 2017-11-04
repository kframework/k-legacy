package org.kframework.parser

import org.kframework.attributes.Source
import org.kframework.parser.concrete2kore.ParseInModule
import org.junit.Test
import org.junit.Assert._
import org.kframework.kore.ADT.SortLookup
import org.kframework.kore._
import org.kframework.definition.Module

import org.kframework.definition.KDefinitionDSL._
import org.kframework.definition.KOREDefinition._
import org.kframework.parser.KOREDowner._


object ExpDefinition {
  import org.kframework.kore.ADT._
  import org.kframework.kore._

  val expString =
    """
      module EXP
        syntax Exp ::= "0" [klabel(0)]
        syntax Exp ::= "1" [klabel(1)]
        syntax Exp ::= "2" [klabel(2)]
        syntax Exp ::= "3" [klabel(3)]
        syntax Exp ::= "4" [klabel(4)]
        syntax Exp ::= "5" [klabel(5)]
        syntax Exp ::= "6" [klabel(6)]
        syntax Exp ::= "7" [klabel(7)]
        syntax Exp ::= "8" [klabel(8)]
        syntax Exp ::= "9" [klabel(9)]

        syntax Exp ::= Exp "+" Exp [klabel(p), plus]
        syntax Exp ::= Exp "-" Exp [minus, klabel(m)]
        syntax Exp ::= Exp "*" Exp [klabel(t), times]
        syntax Exp ::= Exp "/" Exp [klabel(d), div]

        rule 3 + 3 => 6
        rule 9 - 4 => 5
        rule 7 * 0 => 0
      endmodule
    """

  val Exp = SortLookup("Exp")
  val EXP = Module("EXP", imports(), sentences(
    syntax(Exp) is "0" att klabel("0"),
    syntax(Exp) is "1" att klabel("1"),
    syntax(Exp) is "2" att klabel("2"),
    syntax(Exp) is "3" att klabel("3"),
    syntax(Exp) is "4" att klabel("4"),
    syntax(Exp) is "5" att klabel("5"),
    syntax(Exp) is "6" att klabel("6"),
    syntax(Exp) is "7" att klabel("7"),
    syntax(Exp) is "8" att klabel("8"),
    syntax(Exp) is "9" att klabel("9"),

    syntax(Exp) is (Exp, "+", Exp) att(klabel("p"), "plus"),
    syntax(Exp) is (Exp, "-", Exp) att("minus", klabel("m")),
    syntax(Exp) is (Exp, "*", Exp) att(klabel("t"), "times"),
    syntax(Exp) is (Exp, "/", Exp) att(klabel("d"), "div"),

    // priority( >("p", "t") , >("m", "d") ),
    rule(term("p", term("3"), term("3")), term("6")),
    rule(term("m", term("9"), term("4")), term("5")),
    rule(term("t", term("7"), term("0")), term("0"))
  ))
}


class ParserBootstrapTest {
  val kParser = new ParseInModule(KDEFINITION)
  def runParser(parser: ParseInModule, toParse: String, parseAs: SortLookup): K =
    parser.parseString(toParse, parseAs, Source(""))._1 match {
      case Right(x) => x
      case Left(y) => throw new Error("runParser error: " + y.toString)
    }
  def parseK(toParse: String, parseAs: SortLookup): K = runParser(kParser, toParse, parseAs)

  def printInfo(name: String, parsedString: K, origModule: Module, downedModule: Module): Unit = {
    println(name ++ " PARSED AS:")
    println("===================")
    println(parsedString)
    println("===================")
    println(name ++ " ORIG MODULE:")
    println("=====================")
    println(origModule)
    println("=====================")
    println(name ++ " DOWNED MODULE:")
    println("=======================")
    println(downedModule)
    println("=======================")
    println("\n\n")
  }

  @Test def expressionTest(): Unit = {
    import ExpDefinition._
    val parsed = preProcess(parseK(expString, KDefinition))
    val downed = downRules(downModules(parsed, Map.empty)("EXP"))
    //printInfo("EXP", parsed, EXP, downed)
    assertEquals(downRules(EXP), downed)
  }

  @Test def kdefFixpoint(): Unit = {

    //val KORE_STRING = io.Source.fromFile("/Users/lpena/kframework/k/kernel/src/test/scala/org/kframework/parser/kore.k").mkString
    val KORE_STRING = io.Source.fromFile("src/test/scala/org/kframework/parser/kore.k").mkString
    val parsed = preProcess(parseK(KORE_STRING, KDefinition))
    val builtins: Map[String, Module] = Map.empty
    val downed = downModules(parsed, builtins)

    KOREDef.foreach { case (name, module) =>
      // printInfo(name, parsed, module, downed(name))
      assertEquals(downRules(module), downRules(downed(name)))
    }
  }
}
