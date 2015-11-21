package org.kframework.definition

import org.junit.Test
import org.junit.Assert._
import org.kframework.attributes.Att
import org.kframework.kore.ADT._
import org.kframework.utils.errorsystem.KEMException
import collection._

class DefinitionTest {
  val Int = SortLookup("Int")
  val Exp = SortLookup("Exp")
  val INT = Module("INT", Set(), Set(
    SyntaxSort(Int)
  ))
  val IMP = Module("IMP", Set(INT), Set(
    Production(Exp, Seq[ProductionItem](NonTerminal(Int)), Att())))

  @Test def createNewModuleWithSortLookups(): Unit = {
    assertEquals(IMP.Sort("Int"), Sort(INT, "Int"))
    assertEquals(IMP.Sort("Exp"), Sort(IMP, "Exp"))
  }

  @Test def redefineSort(): Unit = {
    val IMP = Module("IMP", Set(INT), Set(
      Production(Exp, Seq[ProductionItem](NonTerminal(Int)), Att()),
      SyntaxSort(Int)
    ))
    assertEquals(IMP.Sort("Int"), Sort(INT, "Int"))
  }

  @Test(expected = classOf[KEMException])
  def testSortNotFound(): Unit = {
    INT.Sort("Exp")
  }
}
