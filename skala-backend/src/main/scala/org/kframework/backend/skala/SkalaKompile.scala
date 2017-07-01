package org.kframework.backend.skala

import java.util.function.Function

import org.kframework.attributes.Att
import org.kframework.compile.{AddBottomSortForListsWithIdenticalLabels, NormalizeKSeq}
import org.kframework.definition.{Definition, DefinitionTransformer, Rule, Sentence}
import org.kframework.kompile.{CompiledDefinition, Kompile, KompileOptions}
import org.kframework.frontend.KORE
import org.kframework.frontend.compile._
import org.kframework.utils.errorsystem.KExceptionManager

class SkalaKompile(kompileOptions: KompileOptions, kem: KExceptionManager) extends Backend {
  override def accept(d: CompiledDefinition): Unit = {
    // probably a redundant function
  }

  override def steps(): Function[Definition, Definition] = d => defaultSteps()(d)

  def defaultSteps(): Definition => Definition = {

    (d => Kompile.defaultSteps(kompileOptions, kem)(d))
      .andThen(DefinitionTransformer.fromRuleBodyTranformer(RewriteToTop.rewriteToTop, "rewrite to top"))
      .andThen(DefinitionTransformer.fromHybrid(AddBottomSortForListsWithIdenticalLabels, "AddBottomSortForListsWithIdenticalLabels"))
      .andThen(Kompile.moduleQualifySortPredicates)
//      .andThen(DefinitionTransformer.fromRuleBodyTranformer(Kompile.ADTKVariableToSortedVariable, "ADT.KVariable to SortedVariable"))
      //      .andThen(new ExpandMacrosDefinitionTransformer(kem, files, globalOptions, kompileOptions))
      .andThen(DefinitionTransformer.fromRuleBodyTranformer(Kompile.convertKSeqToKApply, "kseq to kapply"))
    //      .andThen(DefinitionTransformer.fromRuleBodyTranformer(NormalizeKSeq, "normalize kseq"))
  }
}
