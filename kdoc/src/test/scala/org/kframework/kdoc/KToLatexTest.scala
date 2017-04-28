package org.kframework.kdoc

import org.junit.Test
import org.junit.Assert._
import org.kframework.Collections._
import org.kframework.Parser
import org.kframework.definition.{NonTerminal, Production, Terminal, Module}
import org.kframework.frontend.KORE.Sort
import org.kframework.frontend.Sort
import org.kframework.attributes.Att


class KToLatexTest {
  val xSort: Sort = Sort("X")
  var m: Module = Module.apply("TEST", Set(
    Production(xSort, Seq(Terminal("y"), NonTerminal(xSort)), Att() + ("klabel" -> "y") + (Att.latex -> "\\customY{#1}")),
    Production(xSort, Seq(Terminal("tt"), NonTerminal(xSort), NonTerminal(xSort)), Att() + ("klabel" -> "tt") + (Att.latex -> "\\customTT{#1 #1 #2 #1}")),
    Production(xSort, Seq(Terminal("z"), NonTerminal(xSort)), Att() + ("klabel" -> "z")),
    Production(xSort, Seq(Terminal("x")), Att() + ("klabel" -> "x") + (Att.latex -> "x!"))
  ))

  val parser = Parser.from(m)
  val kToLatex = new KtoLatex(m)

  @Test def withReadingLatexAnnotation() {
    val yx = parser(xSort, "yx")._1.get
    val actual = kToLatex(yx)
    assertEquals("\\customY{x!}", actual)
  }

  @Test def withGeneratingLatexAnnotation() {
    val zx = parser(xSort, "zx")._1.get
    val actual = kToLatex(zx)
    assertEquals("z x!", actual)
  }

  @Test def withRepetitionInPattern() {
    val tt = parser(xSort, "tt y x x")._1.get
    val actual = kToLatex(tt)
    assertEquals("\\customTT{\\customY{x!} \\customY{x!} x! \\customY{x!}}", actual)
  }
}
