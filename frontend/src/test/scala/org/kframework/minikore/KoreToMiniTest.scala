package org.kframework.minikore

import org.junit.Assert.assertEquals
import org.junit.Test
import org.kframework.attributes.Att
import org.kframework.definition
import org.kframework.definition.{ModuleName, NonTerminal}
import org.kframework.kore.implementation.DefaultBuilders._
import org.kframework.frontend.ADT
import org.kframework.frontend.SortedADT.SortedKVariable
import org.kframework.minikore.converters.KoreToMini._

/**
  * Created by daejunpark on 12/28/16.
  */
class KoreToMiniTest {

  val Int = ADT.Sort("Int", ModuleName("INT-SYNTAX"))
  val Exp = ADT.Sort("Exp", ModuleName("A-SYNTAX"))


  @Test def production1(): Unit = {
    val klabelatt = Application(Symbol("klabel"), Seq(DomainValue(Symbol("KString@KSTRING"), Value("_+_"))))
    assertEquals(
      apply(definition.Production.apply("_+_", Exp, Seq(), Att())),
      SymbolDeclaration(Sort(Exp.name), Symbol("_+_"), Seq(), Attributes(Seq(klabelatt)))
    )
  }

  @Test def production2(): Unit = {
    assertEquals(
      apply(new definition.Production(Exp, Seq(NonTerminal(Int)), Att())),
      SymbolDeclaration(Sort(Exp.name), Symbol("#inject_Int@INT-SYNTAX_into_Exp@A-SYNTAX"), Seq(Sort(Int.name)), Attributes(Seq(Application(iNonTerminal, Seq(S(Int.name))))))
    )
  }

  @Test def k1(): Unit = {
    assertEquals(
      apply(SortedKVariable("x", Att())),
      SortedVariable(Name("x"), Sort("K@SORT-K"))
    )
  }
}
