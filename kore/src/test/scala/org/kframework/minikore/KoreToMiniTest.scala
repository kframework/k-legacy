package org.kframework.minikore

import org.junit.Assert.assertEquals
import org.junit.Test
import org.kframework.attributes.Att
import org.kframework.definition
import org.kframework.definition.{ModuleName, NonTerminal}
import org.kframework.kore.ADT.Sort
import org.kframework.kore.SortedADT.SortedKVariable
import org.kframework.minikore.KoreToMini._
import org.kframework.minikore.MiniKore._

/**
  * Created by daejunpark on 12/28/16.
  */
class KoreToMiniTest {

  val Int = Sort("Int", ModuleName("INT-SYNTAX"))
  val Exp = Sort("Exp", ModuleName("A-SYNTAX"))

  @Test def production1(): Unit = {
    val klabelatt = Application("klabel", Seq(DomainValue("KString@KSTRING", "_+_")))
    assertEquals(
      KoreToMini.apply(definition.Production.apply("_+_", Exp, Seq(), Att())),
      SymbolDeclaration(Exp.name, "_+_", Seq(), Seq(klabelatt))
    )
  }

  @Test def production2(): Unit = {
    assertEquals(
      KoreToMini.apply(new definition.Production(Exp, Seq(NonTerminal(Int)), Att())),
      SymbolDeclaration(Exp.name, "", Seq(Int.name), Seq(Application(iNonTerminal, Seq(S(Int.name)))))
    )
  }

  @Test def k1(): Unit = {
    assertEquals(
      KoreToMini.apply(SortedKVariable("x", Att())),
      Variable("x", "K@SORT-K")
    )
  }
}
