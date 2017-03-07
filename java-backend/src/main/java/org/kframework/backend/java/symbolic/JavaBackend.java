// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.backend.java.symbolic;

import org.kframework.AddConfigurationRecoveryFlags;
import org.kframework.Collections;
import org.kframework.KapiGlobal;
import org.kframework.attributes.Att;
import org.kframework.backend.Backends;
import org.kframework.backend.java.kore.compile.ExpandMacrosDefinitionTransformer;
import org.kframework.builtin.KLabels;
import org.kframework.compile.AddBottomSortForListsWithIdenticalLabels;
import org.kframework.compile.NormalizeKSeq;
import org.kframework.compile.ConfigurationInfoFromModule;
import org.kframework.definition.Constructors;
import org.kframework.definition.Definition;
import org.kframework.definition.DefinitionTransformer;
import org.kframework.definition.Module;
import org.kframework.definition.Rule;
import org.kframework.definition.Sentence;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kompile.Kompile;
import org.kframework.kompile.KompileOptions;
import org.kframework.kore.ADT;
import org.kframework.kore.Sort;
import org.kframework.kore.VisitK;
import org.kframework.kore.K;
import org.kframework.kore.KApply;
import org.kframework.kore.KORE;
import org.kframework.kore.KVariable;
import org.kframework.kore.SortedADT;
import org.kframework.kore.compile.AddImplicitComputationCell;
import org.kframework.kore.compile.AssocCommToAssoc;
import org.kframework.kore.compile.Backend;
import org.kframework.kore.compile.ConcretizeCells;
import org.kframework.kore.compile.ConvertDataStructureToLookup;
import org.kframework.kore.compile.MergeRules;
import org.kframework.kore.compile.NormalizeAssoc;
import org.kframework.kore.compile.ResolveAnonVar;
import org.kframework.kore.compile.ResolveSemanticCasts;
import org.kframework.kore.compile.RewriteToTop;
import org.kframework.kore.TransformK;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import scala.Option;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

import static org.kframework.definition.Constructors.Att;
import static scala.runtime.java8.JFunction.func;

public class JavaBackend implements Backend {

    private final KExceptionManager kem;
    private final FileUtil files;
    private final GlobalOptions globalOptions;
    private final KompileOptions kompileOptions;

    @Override
    public void accept(CompiledDefinition def) {
    }

    public JavaBackend(KExceptionManager kem, FileUtil files, GlobalOptions globalOptions, KompileOptions kompileOptions) {
        this.kem = kem;
        this.files = files;
        this.globalOptions = globalOptions;
        this.kompileOptions = kompileOptions;
    }

    public JavaBackend(KapiGlobal kapiGlobal) {
        this(kapiGlobal.kem, kapiGlobal.files, kapiGlobal.globalOptions, kapiGlobal.kompileOptions);
    }

    public static K moduleQualifySortPredicates(Module m, K k) {
        return new TransformK() {
            @Override
            public K apply(KApply kk) {
                KApply k = (KApply) super.apply(kk);
                if (!k.klabel().name().startsWith("is"))
                    return k;

                Sort possibleSort = KORE.Sort(k.klabel().name().substring("is".length()));
                Option<ADT.Sort> resolvedSort = m.sortResolver().get(possibleSort);

                if (resolvedSort.isDefined()) {
                    return KORE.KApply(KORE.KLabel("is" + resolvedSort.get().name()), k.klist(), kk.att());
                } else {
                    return k;
                }
            }
        }.apply(k);
    }

    /**
     * @param the generic {@link Kompile}
     * @return the special steps for the Java backend
     */
    @Override
    public Function<Definition, Definition> steps() {
        DefinitionTransformer convertDataStructureToLookup = DefinitionTransformer.fromSentenceTransformer(func((m, s) -> new ConvertDataStructureToLookup(m, false).convert(s)), "convert data structures to lookups");
        ExpandMacrosDefinitionTransformer expandMacrosDefinitionTransformer = new ExpandMacrosDefinitionTransformer(kem, files, globalOptions, kompileOptions);

        return d -> (func((Definition dd) -> Kompile.defaultSteps(kompileOptions, kem).apply(dd)))
                .andThen(DefinitionTransformer.fromRuleBodyTranformer(RewriteToTop::bubbleRewriteToTopInsideCells, "bubble out rewrites below cells"))
                .andThen(DefinitionTransformer.fromSentenceTransformer(new NormalizeAssoc(KORE.c()), "normalize assoc"))
                .andThen(AddBottomSortForListsWithIdenticalLabels.singleton().lift())
                .andThen(DefinitionTransformer.fromKTransformerWithModuleInfo(JavaBackend::moduleQualifySortPredicates, "Module-qualify sort predicates"))
                .andThen(expandMacrosDefinitionTransformer::apply)
                .andThen(DefinitionTransformer.fromSentenceTransformer(new NormalizeAssoc(KORE.c()), "normalize assoc"))
                .andThen(convertDataStructureToLookup)
                .andThen(DefinitionTransformer.fromRuleBodyTranformer(JavaBackend::ADTKVariableToSortedVariable, "ADT.KVariable to SortedVariable"))
                .andThen(DefinitionTransformer.fromRuleBodyTranformer(Kompile::convertKSeqToKApply, "kseq to kapply"))
                .andThen(DefinitionTransformer.fromRuleBodyTranformer(NormalizeKSeq.self(), "normalize kseq"))
                .andThen(JavaBackend::markRegularRules)
                .andThen(DefinitionTransformer.fromSentenceTransformer(new AddConfigurationRecoveryFlags()::apply, "add refers_THIS_CONFIGURATION_marker"))
                .andThen(DefinitionTransformer.fromSentenceTransformer(JavaBackend::markSingleVariables, "mark single variables"))
                .andThen(new AssocCommToAssoc(KORE.c()).lift())
                .andThen(new MergeRules(KORE.c()).lift())
                .andThen(DefinitionTransformer.fromKTransformerWithModuleInfo(JavaBackend::moduleQualifySortPredicates, "Module-qualify sort predicates"))
             // .andThen(KoreToMiniToKore::apply) // for serialization/deserialization test
                .apply(d);
    }

    public Function<Definition, Definition> stepsForProverRules() {
        ExpandMacrosDefinitionTransformer expandMacrosDefinitionTransformer = new ExpandMacrosDefinitionTransformer(kem, files, globalOptions, kompileOptions);

        return d -> new ResolveAnonVar().lift()
                .andThen(new ResolveSemanticCasts(kompileOptions.backend.equals(Backends.JAVA)).lift())
                .andThen(AddImplicitComputationCell::transformDefinition)
                .andThen(ConcretizeCells::transformDefinition)
                .andThen(DefinitionTransformer.fromRuleBodyTranformer(RewriteToTop::bubbleRewriteToTopInsideCells, "bubble out rewrites below cells"))
                .andThen(AddBottomSortForListsWithIdenticalLabels.singleton().lift())
                .andThen(DefinitionTransformer.fromKTransformerWithModuleInfo(JavaBackend::moduleQualifySortPredicates, "Module-qualify sort predicates"))
                .andThen(expandMacrosDefinitionTransformer::apply)
                .andThen(DefinitionTransformer.fromRuleBodyTranformer(JavaBackend::ADTKVariableToSortedVariable, "ADT.KVariable to SortedVariable"))
                .andThen(DefinitionTransformer.fromRuleBodyTranformer(Kompile::convertKSeqToKApply, "kseq to kapply"))
                .andThen(DefinitionTransformer.fromRuleBodyTranformer(NormalizeKSeq.self(), "normalize kseq"))
                .andThen(JavaBackend::markRegularRules)
                .andThen(DefinitionTransformer.fromSentenceTransformer(new AddConfigurationRecoveryFlags()::apply, "add refers_THIS_CONFIGURATION_marker"))
                .apply(d);
    }

    /**
     * Put a marker on the "regular" (i.e. non function/macro/etc.) rules that we can use later.
     */
    private static Definition markRegularRules(Definition d) {
        ConfigurationInfoFromModule configInfo = new ConfigurationInfoFromModule(d.mainModule());
        return DefinitionTransformer.fromSentenceTransformer((Sentence s) -> {
            if (s instanceof org.kframework.definition.Rule) {
                org.kframework.definition.Rule r = (org.kframework.definition.Rule) s;
                if (r.body() instanceof KApply && d.mainModule().sortFor().apply(((KApply) r.body()).klabel()).equals(configInfo.topCell())) {
                    return org.kframework.definition.Rule.apply(r.body(), r.requires(), r.ensures(), r.att().add(Att.topRule()));
                } else
                    return r;
            } else
                return s;
        }, "mark regular rules").apply(d);
    }

    /**
     * The Java backend expects sorted variables, so transform them to the sorted flavor.
     */
    public static K ADTKVariableToSortedVariable(K ruleBody) {
        return new TransformK() {
            public K apply(KVariable kvar) {
                return new SortedADT.SortedKVariable(kvar.name(), kvar.att());
            }
        }.apply(ruleBody);
    }

    /**
     * Replace variables which only appear once in the pattern and have no side condition on them (including no sorting),
     * with a special marker called THE_VARIABLE which the backend uses for special speed optimisations.
     */
    private static Sentence markSingleVariables(Sentence s) {
        if (s instanceof Rule) {
            Rule r = (Rule) s;

            if (!r.att().contains(Att.topRule()))
                return r;

            Map<KVariable, Integer> varCount = new HashMap<>();
            VisitK markerVisitor = new VisitK() {
                public void apply(KVariable kvar) {
                    varCount.put(kvar, varCount.getOrDefault(kvar, 0) + 1);
                }
            };
            markerVisitor.apply(r.body());
            markerVisitor.apply(r.requires());
            markerVisitor.apply(r.ensures());

            TransformK markerAdder = new TransformK() {
                public K apply(KVariable kvar) {
                    if (kvar instanceof SortedADT.SortedKVariable && ((SortedADT.SortedKVariable) kvar).sort().equals(KORE.Sort("K")) && varCount.get(kvar) == 1
                            && !kvar.name().equals(KLabels.THIS_CONFIGURATION)) {
                        return new SortedADT.SortedKVariable("THE_VARIABLE", Att());
                    } else {
                        return kvar;
                    }
                }
            };

            return Constructors.Rule(markerAdder.apply(r.body()), markerAdder.apply(r.requires()), markerAdder.apply(r.ensures()), r.att());
        } else {
            return s;
        }
    }
}
