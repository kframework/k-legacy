package org.kframework.parser

import org.kframework.attributes.{Source, Att}
import org.kframework.parser.concrete2kore.ParseInModule
import org.junit.Test
import org.junit.Assert._
import org.kframework.minikore.{KoreToMini, MiniToKore}
import org.kframework.kore.ADT.SortLookup
//import org.kframework.kore._
//import org.kframework.definition.Module

import org.kframework.kore.KORE
import org.kframework.minikore.MiniKore._
import org.kframework.minikore.KDefinitionDSL._
import org.kframework.minikore.KOREDefinition._
import org.kframework.parser.MiniKoreMeta._
import org.kframework.parser.MetaPasses._


object ExpDefinition {

  val expSentence = """syntax Exp ::= Stmt "true""""

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

  val Exp = Sort("Exp")
  val Stmt = Sort("Stmt")
  val SubExp1 = Sort("SubExp1")
  val SubExp2 = Sort("SubExp2")
  val EXP: Module = module("EXP",
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
    syntax(Exp) is (Exp, "/", Exp) att(klabel("d"), "div")

    // priority( >("p", "t") , >("m", "d") ),
    // rule(term("p", term("3"), term("3")), term("6")),
    // rule(term("m", term("9"), term("4")), term("5")),
    // rule(term("t", term("7"), term("0")), term("0"))
  )

  val EXP_DEF = Definition(Seq(EXP), Seq(attribute(KoreToMini.iMainModule, "EXP"), attribute(KoreToMini.iEntryModules, "EXP")))
}

class ParserBootstrapTest {
  import MiniKoreStaging._
  val miniDef = MiniToKore(onAttributesDef(traverseTopDown(toMiniKoreEncoding))(KOREDef))
  val mainMod = miniDef.mainModule
  val kParser = new ParseInModule(mainMod)

  def runParser(parser: ParseInModule, toParse: String, parseAs: String): Pattern =
    parser.parseString(toParse, SortLookup(parseAs), Source(""))._1 match {
      case Right(x) => KoreToMini(x)
      case Left(y) => throw new Error("runParser error: " + y.toString)
    }
  def parseK(toParse: String, parseAs: String): Pattern = runParser(kParser, toParse, parseAs)

  def printInfo(parsedPattern: Pattern, origModule: Module, downedModule: Module): Unit = {
    println("PARSED AS:")
    println("===================")
    println(parsedPattern)
    println("===================")
    println("ORIG MODULE:")
    println("=====================")
    println(origModule)
    println("=====================")
    println("DOWNED MODULE:")
    println("=======================")
    println(downedModule)
    println("=======================")
    println("\n\n")
  }

  def expressionTest(): Unit = {
    import ExpDefinition._
//    println("==================")
//    println(KOREDef)
//    println("==================")
//    println(MiniToKore(KOREDef))
//    println("==================")
    val parsed = parseK(expSentence, "KSentence")
    println(preProcess(parsed))
    //val parsed2 = preProcess(parseK(zeroProduction, "KProduction"))
    //val downed = downModules(parsed)
    //printInfo("EXP", parsed, EXP, downed)
    //assertEquals(Seq(EXP), downed)
  }

  def profiling(): Unit = {
    val KORE_STRING = io.Source.fromFile("src/test/scala/org/kframework/parser/kore.k").mkString
    val KORE40_STRING = io.Source.fromFile("src/test/scala/org/kframework/parser/kore40.k").mkString
    val start = System.currentTimeMillis()
    val parsed = parseK(KORE_STRING, "KDefinition")
    val start40 = System.currentTimeMillis()
    val parsed40 = parseK(KORE40_STRING, "KDefinition")
    val end = System.currentTimeMillis()
    println("Parsing once: " + (start40 - start))
    println("Parsing 40 times: " + (end - start40))

  }

  def kdefFixpoint(): Unit = {
    val KORE_STRING = io.Source.fromFile("src/test/scala/org/kframework/parser/kore.k").mkString
    val parsed = preProcess(parseK(KORE_STRING, "KDefinition"))
    println(parsed)
    val downed = downDefinition(parsed)
    println()
    println("----- DOWNED -----")
    println(downed)
    assertEquals(KOREDef.modules map { case Module(name, _, _) => name }, downed.modules map { case Module(name, _, _) => name })
    KOREDef.modules foreach { case Module(name, sentences, atts) =>
      val (dSents, dAtts) = downed.modules collect { case Module(`name`, downedSentences, downedAtts) => (downedSentences, downedAtts) } head;
      println("MODULE: " + name)
      assertEquals(sentences, dSents)
      assertEquals(atts, dAtts)
    }
  }

  @Test def sentenceTest(): Unit = {
    import ExpDefinition._
    val sentenceString = """syntax Exp := mystmt(Stmt) [klabel(mystmt), production(#NonTerminal(Stmt))]"""
    val sentenceConcrete: SymbolDeclaration = syntax(Exp) is Stmt att klabel("mystmt")
    println(sentenceConcrete)
    val parsed = preProcess(parseK(sentenceString, "KSentence"))
    println(parsed)
    val downed = downSentence(parsed)
    assertEquals(sentenceConcrete, downed)

  }

}
