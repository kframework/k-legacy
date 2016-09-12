package org.kframework.definition

import org.junit.Test
import org.junit.Assert._


class ResolvedSymbolTest {

  case class L(localName: String, moduleName: ModuleName) extends LookupSymbol

  case class S(localName: String, moduleName: ModuleName) extends ResolvedSymbol

  implicit val makeS = S
  implicit val makeL = L

  val ns = ModuleName

  val INT = SymbolResolver[L, S]("INT", Set(), Set(L("Int", ns("INT"))))

  implicit class WithEqualsAssert[T](actual: T) {
    def ===(expected: T) = assertEquals(expected, actual)
  }

  @Test def cleanExtends() {
    val IMP = SymbolResolver[L, S]("IMP", Set(INT), Set(L("Exp", ns("IMP"))))

    IMP.defined === Set(S("Exp", ns("IMP")))

    IMP.get(L("Exp", ns("IMP"))) === Some(S("Exp", ns("IMP")))
    IMP.get(L("Int", ns("INT"))) === Some(S("Int", ns("INT")))
    IMP.get(L("Int", ns("IMP"))) === Some(S("Int", ns("INT")))
    IMP.get(L("Exp", ns("INT"))) === None
    IMP.get(L("Foo", ns("IMP"))) === None
    IMP.get(L("Foo", ns("INT"))) === None
    IMP.get(L("Exp", ns("FOO"))) === None
    IMP.get(L("Int", ns("FOO"))) === None
  }

  @Test def overrideExtends() {
    val IMP = SymbolResolver[L, S]("IMP", Set(INT), Set(L("Int", ns("IMP"))))

    IMP.defined === Set()

    IMP.get(L("Int", ns("INT"))) === Some(S("Int", ns("INT")))
    IMP.get(L("Int", ns("IMP"))) === Some(S("Int", ns("INT")))
    IMP.get(L("Foo", ns("IMP"))) === None
    IMP.get(L("Foo", ns("INT"))) === None
    IMP.get(L("Int", ns("FOO"))) === None
  }

  @Test def definingInImportedModule() {
    val IMP = SymbolResolver[L, S]("IMP", Set(INT), Set(L("Foo", ns("INT"))))

    IMP.defined === Set(S("Foo", ns("INT")))

    IMP.get(L("Int", ns("INT"))) === Some(S("Int", ns("INT")))
    IMP.get(L("Int", ns("IMP"))) === Some(S("Int", ns("INT")))
    IMP.get(L("Foo", ns("INT"))) === Some(S("Foo", ns("INT")))
    IMP.get(L("Foo", ns("IMP"))) === Some(S("Foo", ns("INT")))
  }

  @Test def definingJunk() {
    val IMP = SymbolResolver[L, S]("IMP", Set(INT), Set(L("Foo", ns("FOO"))))

    IMP.defined === Set(S("Foo", ns("FOO")))

    IMP.get(L("Int", ns("INT"))) === Some(S("Int", ns("INT")))
    IMP.get(L("Int", ns("IMP"))) === Some(S("Int", ns("INT")))
    IMP.get(L("Foo", ns("FOO"))) === Some(S("Foo", ns("FOO")))
    IMP.get(L("Foo", ns("IMP"))) === Some(S("Foo", ns("FOO")))
    IMP.get(L("Foo", ns("INT"))) === None
  }
}
