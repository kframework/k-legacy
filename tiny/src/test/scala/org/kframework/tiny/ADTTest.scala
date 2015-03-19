package org.kframework.tinytest

import org.junit.{Ignore, Test}
import org.kframework.builtin.Sorts
import org.kframework.definition.CrazyModule
import org.kframework.kore.ADT
import org.kframework.tiny.AbstractTest
import org.kframework.tiny.builtin.KMapAppLabel
import org.kframework.{builtin => m}

class ADTTest extends AbstractTest {

  import cons._
  import org.kframework.builtin.Labels
  import org.kframework.tiny._
  import sugar._

  val labels = new Labels(cons)

  val seqX2 = X ~> 2
  val rew = (1: K) ==> 2


  @Test def equalities {
    assertEquals(2: K, 2: K)
    assertEquals(X, KVariable("X"))
    assertEquals(seqX2, X ~> 2)
    assertNotEquals(seqX2, X ~> X ~> 2)
    assertEquals(KSequence(X, X, 2), KSequence(X, KSequence(X, 2)))
    assertEquals(KSequence(X, X, 2), KSequence(KSequence(X, X), 2))
    assertEquals('foo(), KLabel("foo")())
    assertNotEquals('foo(), KLabel("bar")())
    assertNotEquals('foo(), KLabel("foo")(X))



    val divide = NativeBinaryOpLabel(m.Int./, Att(), (x: Int, y: Int) => x / y, Sorts.Int)
    val mapLabel = KMapAppLabel(ADT.KLabel("Map", CrazyModule))

    assertEquals(5: K, divide(10, 2).normalize)
    assertEquals(KSequence(5: K), KSequence(divide(10, 2)).normalize)
    assertEquals('foo(KSequence(5: K)), 'foo(KSequence(divide(10, 2))).normalize)
    assertEquals('foo(KSequence(5: K)), And('foo(divide(10, 2))).normalize)
    assertEquals('+(KSequence(5: K)), And('+(KSequence(divide(10, 2)))).normalize)
    assertEquals('+('+(KSequence(5: K)), '+(mapLabel())), '+('+(KSequence(divide
      (10, 2))), '+(mapLabel())
    ).normalize)


  }

  @Ignore @Test def AndMatcher {
    assertEquals(True, And().matcher(KToken(Sorts.Bool, "true", Att())).normalize)
    assertEquals(And(), And().matcher(And()).normalize)

    assertEquals(Or(1: K, 2: K), (1: K) | 2)
  }
}
