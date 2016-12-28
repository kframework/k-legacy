package org.kframework.minikore

import org.kframework.definition

object KoreToMiniToKore extends (definition.Definition => definition.Definition) {
  def apply(d: definition.Definition): definition.Definition = {
    d
  }
}
