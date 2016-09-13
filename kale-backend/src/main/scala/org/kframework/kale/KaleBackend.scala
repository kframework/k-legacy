package org.kframework.kale

import java.util
import java.util.function.Function

import org.kframework.Strategy
import org.kframework.backend.Backends
import org.kframework.compile.{AddBottomSortForListsWithIdenticalLabels, NormalizeKSeq}
import org.kframework.definition.{Definition, DefinitionTransformer, Module, Sentence}
import org.kframework.kompile.{CompiledDefinition, Kompile}
import org.kframework.kore.KORE
import org.kframework.kore.compile.{GenerateSortPredicateSyntax, _}

class KaleBackend extends Backend {
  override def accept(d: CompiledDefinition): Unit = {
    // probably a redundant function
  }

  override def steps(kompile: Kompile): Function[Definition, Definition] = d => defaultSteps(kompile)(d)

  def defaultSteps(kompile: Kompile): Definition => Definition = {
    val kompileOptions = kompile.kompileOptions


    (d => kompile.defaultSteps()(d))
      .andThen(DefinitionTransformer.fromRuleBodyTranformer(RewriteToTop.bubbleRewriteToTopInsideCells, "bubble out rewrites below cells"))
      .andThen(DefinitionTransformer.fromSentenceTransformer(new NormalizeAssoc(KORE), "normalize assoc"))
      .andThen(DefinitionTransformer.fromHybrid(AddBottomSortForListsWithIdenticalLabels, "AddBottomSortForListsWithIdenticalLabels"))
      .andThen(Kompile.moduleQualifySortPredicates)
      //      .andThen(new ExpandMacrosDefinitionTransformer(kem, files, globalOptions, kompileOptions))
      .andThen(DefinitionTransformer.fromSentenceTransformer(new NormalizeAssoc(KORE), "normalize assoc"))
      //      .andThen(DefinitionTransformer.fromRuleBodyTranformer(ADTKVariableToSortedVariable, "ADT.KVariable to SortedVariable"))
      .andThen(DefinitionTransformer.fromRuleBodyTranformer(Kompile.convertKSeqToKApply, "kseq to kapply"))
      .andThen(DefinitionTransformer.fromRuleBodyTranformer(NormalizeKSeq, "normalize kseq"))
  }
}
