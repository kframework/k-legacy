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

  @Test def simpleLookups(): Unit = {
    assertEquals(Sort(INT, "Int"), IMP.Sort("Int"))
    assertEquals(Sort(IMP, "Exp"), IMP.Sort("Exp"))
  }

  @Test def redefineSort(): Unit = {
    val IMP = Module("IMP", Set(INT), Set(
      Production(Exp, Seq[ProductionItem](NonTerminal(Int)), Att()),
      SyntaxSort(Int)
    ))
    assertEquals(Sort(INT, "Int"), IMP.Sort("Int"))
  }

  @Test def transitiveLookup(): Unit = {
    assertEquals(Set(Sort(INT, "Int")), INT.lookupSort("Int", "INT"))
    assertEquals(Set(Sort(INT, "Int")), IMP.lookupSort("Int", "IMP"))
    assertEquals(Sort(INT, "Int"), IMP.Sort("Int@IMP"))
  }

  @Test(expected = classOf[KEMException])
  def testSortNotFound(): Unit = {
    INT.Sort("Exp")
  }
}
