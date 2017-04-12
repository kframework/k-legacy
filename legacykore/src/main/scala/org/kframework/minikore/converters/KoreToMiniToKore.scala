package org.kframework.minikore.converters

import org.kframework.definition

object KoreToMiniToKore {
  def apply(d: definition.Definition): definition.Definition = {
    val m = KoreToMini.apply(d)
    val k = MiniToKore.apply(m)
    assert(d == k)
    k
  }
}
