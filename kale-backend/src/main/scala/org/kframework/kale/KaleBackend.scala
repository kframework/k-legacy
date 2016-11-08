package org.kframework.kale

import java.util.function.Function

import org.kframework.compile.{AddBottomSortForListsWithIdenticalLabels, NormalizeKSeq}
import org.kframework.definition.{Definition, DefinitionTransformer}
import org.kframework.kompile.{CompiledDefinition, Kompile, KompileOptions}
import org.kframework.kore.KORE
import org.kframework.kore.compile._
import org.kframework.utils.errorsystem.KExceptionManager
import collection.JavaConverters._

class KaleBackend(kompileOptions: KompileOptions, kem: KExceptionManager) extends Backend {
  override def accept(d: CompiledDefinition): Unit = {
    // probably a redundant function
  }

  override def steps(): Function[Definition, Definition] = { dd: Definition =>
    var d: Definition = new ResolveIOStreams(dd, kem).apply(dd)
    d = new ConvertStrictToContexts(kompileOptions).apply(d)
    d = new ResolveAnonVar().apply(d)
    d = new ConvertContextsToHeatCoolRules(kompileOptions).resolve(d)
    d = new ResolveHeatCoolAttribute(kompileOptions.transition.asScala.toSet.asJava).apply(d)
    d = new ResolveSemanticCasts(false).apply(d)
    d = DefinitionTransformer.fromWithInputDefinitionTransformerClass(classOf[GenerateSortPredicateSyntax]).apply(d)
    d = Kompile.resolveFreshConstants(d)
    d = AddImplicitComputationCell.transformDefinition(d)
    d = ConcretizeCells.transformDefinition(d)
    d = Kompile.addSemanticsModule(d)
    d = DefinitionTransformer.fromWithInputDefinitionTransformerClass(classOf[GenerateSortPredicateSyntax])(d)
    d = DefinitionTransformer.fromRuleBodyTranformer(RewriteToTop.rewriteToTop, "rewrite to top")(d)
    d = DefinitionTransformer.fromSentenceTransformer(new NormalizeAssoc(KORE), "normalize assoc")(d)
    d = DefinitionTransformer.fromHybrid(AddBottomSortForListsWithIdenticalLabels, "AddBottomSortForListsWithIdenticalLabels")(d)
    d = Kompile.moduleQualifySortPredicates(d)
    d = DefinitionTransformer.fromSentenceTransformer(new NormalizeAssoc(KORE), "normalize assoc")(d)
    d = DefinitionTransformer.fromRuleBodyTranformer(Kompile.convertKSeqToKApply, "kseq to kapply")(d)
    d = DefinitionTransformer.fromRuleBodyTranformer(NormalizeKSeq, "normalize kseq")(d)
    d
  }
}
