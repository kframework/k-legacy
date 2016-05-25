package org.kframework.kale

import java.util
import java.util.function.Function

import org.kframework.Strategy
import org.kframework.backend.Backends
import org.kframework.definition.{Definition, DefinitionTransformer, Module, Sentence}
import org.kframework.kompile.{CompiledDefinition, Kompile}
import org.kframework.kore.compile.{GenerateSortPredicateSyntax, _}

class KaleBackend extends Backend {
  override def accept(d: CompiledDefinition): Unit = {
    // probably a redundant function
  }

  override def steps(kompile: Kompile): Function[Definition, Definition] = defaultSteps(kompile)

  def defaultSteps(kompile: Kompile): Function[Definition, Definition] = {
    val kompileOptions = kompile.kompileOptions
    val resolveStrict: DefinitionTransformer = DefinitionTransformer.from({ m: Module => new ResolveStrict(kompileOptions).resolve(m) }, "resolving strict and seqstrict attributes")
    val resolveHeatCoolAttribute: DefinitionTransformer = DefinitionTransformer.fromSentenceTransformer({ s: Sentence => new ResolveHeatCoolAttribute(new util.HashSet[String](kompileOptions.transition)).resolve(s) }, "resolving heat and cool attributes")
    val resolveAnonVars: DefinitionTransformer = DefinitionTransformer.fromSentenceTransformer({ s: Sentence => new ResolveAnonVar().resolve(s) }, "resolving \"_\" vars")
    val resolveSemanticCasts: DefinitionTransformer = DefinitionTransformer.fromSentenceTransformer({ s: Sentence => new ResolveSemanticCasts(kompileOptions.backend == Backends.JAVA).resolve(s) }, "resolving semantic casts")
    val generateSortPredicateSyntax: DefinitionTransformer = DefinitionTransformer.from({ m: Module => new GenerateSortPredicateSyntax().gen(m) }, "adding sort predicate productions")

    new Function[Definition, Definition] {
      override def apply(d: Definition): Definition =
        (kompile.resolveIOStreams _)
          .andThen(resolveStrict)
          .andThen(resolveAnonVars)
          .andThen(new ResolveContexts(kompileOptions).resolve _)
          .andThen(resolveHeatCoolAttribute)
          .andThen(resolveSemanticCasts)
          .andThen(generateSortPredicateSyntax)
          .andThen(kompile.resolveFreshConstants _)
          .andThen(AddImplicitComputationCell.transformDefinition)
          .andThen(new Strategy(kompileOptions.experimental.heatCoolStrategies).addStrategyCellToRulesTransformer)
          .andThen(ConcretizeCells.transformDefinition)
          .andThen(kompile.addSemanticsModule)(d)
    }
  }
}
