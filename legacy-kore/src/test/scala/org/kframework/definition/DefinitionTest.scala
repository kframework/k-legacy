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
    assertEquals(Sort("Int", ModuleName("INT")), IMP.Sort("Int"))
    assertEquals(Sort("Exp", ModuleName("IMP")), IMP.Sort("Exp"))
  }

  @Test def redefineSort(): Unit = {
    val IMP = Module("IMP", Set(INT), Set(
      Production(Exp, Seq[ProductionItem](NonTerminal(Int)), Att()),
      SyntaxSort(Int)
    ))
    assertEquals(Sort("Int", ModuleName("INT")), IMP.Sort("Int"))
  }

  @Test def transitiveLookup(): Unit = {
    assertEquals(Sort("Int", ModuleName("INT")), INT.Sort("Int"))
    assertEquals(Sort("Int", ModuleName("INT")), IMP.Sort("Int"))
  }

  @Test(expected = classOf[KEMException])
  def testSortNotFound(): Unit = {
    INT.Sort("Exp")
  }
}
