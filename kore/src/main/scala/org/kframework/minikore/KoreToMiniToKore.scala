package org.kframework.minikore

import org.kframework.definition

object KoreToMiniToKore {
  def apply(d: definition.Definition): definition.Definition = {
    MiniToKore.apply(KoreToMini.apply(d))
  }
}
