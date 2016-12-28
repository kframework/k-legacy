package org.kframework.minikore

import org.kframework.definition.Definition

object KoreToMiniToKore extends (Definition => Definition) {
  override def apply(d: Definition): Definition = {
    d
  }
}
