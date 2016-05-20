package org.kframework.kale

import java.util.function.Function

import org.kframework.definition.Definition
import org.kframework.kompile.{CompiledDefinition, Kompile}
import org.kframework.kore.compile.Backend

class KaleBackend extends Backend {
  override def accept(d: CompiledDefinition): Unit = {}

  override def steps(kompile: Kompile): Function[Definition, Definition] = kompile.defaultSteps()
}
