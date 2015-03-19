package org.kframework.tiny

import org.kframework.tiny.builtin.{Tup2, KMapApp, KVarMapValue}
import org.kframework.tiny.matcher.Anywhere

object Substitution {
  implicit def apply(self: K): Substitution = new Substitution(self)
}

class Substitution(self: K) {

  import org.kframework.tiny.Substitution._

  def substitute(substitution: Map[KVar, K]): K = {
    if (self.isGround || substitution.isEmpty)
      self
    else
      doSubstitution(substitution)
  }

  private def doSubstitution(substitution: Map[KVar, K]) =
    self match {
      case v: KVar => substitution.get(v) map { _.substitute(substitution) } getOrElse v
      case a@Anywhere(name, p, _) => substitution(a.TOPVariable).substitute(substitution + (a.HOLEVariable -> p))
      case b: Binding => b.klabel(b.variable, b.value.substitute(substitution))
      case k: KMapApp =>
        val newChildren = k.children map {
          case KApp(`Tup2`, Seq(k: KVar, KVarMapValue), _) =>
            substitution.get(k).getOrElse(Tup2(k, KVarMapValue))
          case KApp(`Tup2`, Seq(k, v), _) =>
            Tup2(k.substitute(substitution), v.substitute(substitution))
        }

        k.klabel(newChildren.toSeq: _*)
      case KApp(l, ks, att) =>
        l(ks map {x: K => x.substitute(substitution) }, att)
      case e => e
    }
}
