package org.kframework.parser

import org.kframework.attributes.Source
import org.kframework.parser.concrete2kore.ParseInModule
import org.junit.Test
import org.junit.Assert._
import org.kframework.kore.ADT.SortLookup

import org.kframework.minikore.MiniKore._
import org.kframework.minikore.KoreToMini
import org.kframework.minikore.MiniToKore
import org.kframework.minikore.MiniKoreMeta._
import org.kframework.minikore.MiniKoreOuterUtils._
import org.kframework.minikore.MiniKorePatternUtils._

import org.kframework.parser.KDefinitionDSL._
import org.kframework.parser.KOREDefinition._
import org.kframework.parser.EKOREDefinition._
import org.kframework.parser.ParserNormalization._

object ExpDefinition {

  val expString =
    """
      [ #MainModule(EXP)
      , #EntryModules(EXP)
      ]

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

        rule p(3, 3) => 6
        rule m(9, 4) => 5
        rule t(7, 0) => 0
      endmodule
    """

  val Exp = Sort("Exp")
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
    syntax(Exp) is (Exp, "/", Exp) att(klabel("d"), "div"),

    // priority( >("p", "t") , >("m", "d") ),
    rule(term("p", term("3"), term("3")), term("6")),
    rule(term("m", term("9"), term("4")), term("5")),
    rule(term("t", term("7"), term("0")), term("0"))
  )

  val EXP_DEF = definition(EXP) att(application(KoreToMini.iMainModule, "EXP"), application(KoreToMini.iEntryModules, "EXP"))
}

class ParserBootstrapTest {

  val miniDef = MiniToKore(onAttributesDef(traverseTopDown(toKoreEncoding))(EKORE))
  val mainMod = miniDef.mainModule
  val kParser = new ParseInModule(mainMod)

  def runParser(parser: ParseInModule, toParse: String, parseAs: String): Pattern =
    parser.parseString(toParse, SortLookup(parseAs), Source(""))._1 match {
      case Right(x) => KoreToMini(x)
      case Left(y) => throw new Error("runParser error: " + y.toString)
    }
  def parseK(toParse: String, parseAs: String): Pattern = runParser(kParser, toParse, parseAs)

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

  @Test def kdefFixpoint(): Unit = {
    val KORE_STRING = io.Source.fromFile("src/test/scala/org/kframework/parser/kore.k").mkString
    val parsed = ekoreToKore(preProcess(parseK(KORE_STRING, "KDefinition")))
    val downed = downDefinition(parsed)
    assertEquals(KORE, downed)
  }

  @Test def sentenceTest(): Unit = {
    val Exp  = Sort("Exp")
    val Stmt = Sort("Stmt")
    val sentenceTests: Seq[(Sentence, String)]
        = Seq( (symbol(Exp, "mystmt", Stmt)               , """syntax Exp := mystmt(Stmt)"""                                                                     )
             , (symbol(Exp, "_", Stmt) att kprod(Stmt)    , """syntax Exp ::= Stmt"""                                                                            )
             , (syntax(Exp) is Stmt att klabel("mystmt")  , """syntax Exp := mystmt(Stmt) [klabel(mystmt), production(KNonTerminal@K-PRETTY-PRODUCTION(Stmt))]""")
             , (syntax(Exp) is Stmt                       , """syntax Exp ::= Stmt"""                                                                            )
             , (syntax(Exp) is Stmt att klabel("mystmt")  , """syntax Exp ::= Stmt [klabel(mystmt)]"""                                                           )
             , (syntax(Exp) is ("true", Stmt)             , """syntax Exp ::= "true" Stmt"""                                                                     )
             , (syntax(Exp) is Regex("[^ \n\r\t]+")       , """syntax Exp ::= r"[^ \n\r\t]+""""                                                                  )
             , (syntax(Exp) is Regex(" a\n\r\tb")         , """syntax Exp ::= r" a\n\r\tb""""                                                                    )
             , (syntax(Exp) is Regex("`[^ a\n\r\tb]+`")   , """syntax Exp ::= r"`[^ a\n\r\tb]+`""""                                                              )
             )

    sentenceTests foreach { sentStr =>
      val parsed = ekoreToKore(preProcess(parseK(sentStr._2, "KSentence")))
      assertEquals(sentStr._1, downSentence(parsed))
    }
  }

  @Test def prettyProductionsTest(): Unit = {
    val prettyTests: Seq[(String, String)]
        = Seq( ("""syntax Exp ::= "true" syntax Exp ::= Exp""", """syntax Exp ::= "true" | Exp""") )

    def parseAndDown(input: String): Seq[Sentence] = flattenByLabels("KSentenceList", ".KSentenceList")(ekoreToKore(preProcess(parseK(input, "KSentenceList")))) map downSentence
    prettyTests foreach { strings =>
      assertEquals(parseAndDown(strings._1), parseAndDown(strings._2))
    }
  }

  @Test def abstractRulesTest(): Unit = {
    import ExpDefinition._

    val parsed = ekoreToKore(preProcess(parseK(expString, "KDefinition")))
    val downed = downDefinition(parsed)
    assertEquals(EXP_DEF, downed)
  }

}
