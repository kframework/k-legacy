package org.kframework.minikore

import org.junit.Test
import org.junit.Assert.{assertEquals, _}
import org.kframework.attributes.Att
import org.kframework.definition
import org.kframework.attributes
import org.kframework.definition.{ModuleName, NonTerminal}
import org.kframework.kore.ADT
import org.kframework.kore.ADT.Sort
import org.kframework.minikore.MiniKore._
import org.kframework.minikore.KoreToMini

/**
  * Created by daejunpark on 12/28/16.
  */
class KoreToMiniTest {

  val Int = Sort("Int", ModuleName("INT-SYNTAX"))
  val Exp = Sort("Exp", ModuleName("A-SYNTAX"))

  @Test def production1(): Unit = {
    assertEquals(
      KoreToMini.apply(definition.Production.apply("_+_", Exp, Seq(), Att())),
      Syntax(Exp.name, "_+_", Seq(), Seq())
    )
  }

  @Test def production2(): Unit = {
    assertEquals(
      KoreToMini.apply(new definition.Production(Exp, Seq(NonTerminal(Int)), Att())),
      Syntax(Exp.name, "construct" + Exp.name, Seq(Int.name), Seq(Term("NonTerminal", Seq(S(Int.name)))))
    )
  }
}
