// Copyright (c) 2015 K Team. All Rights Reserved.
package org.kframework.backend.java.symbolic;

import com.google.common.collect.Iterables;
import com.google.inject.Inject;
import com.google.inject.Provider;
import org.kframework.Rewriter;
import org.kframework.RewriterResult;
import org.kframework.backend.java.compile.KOREtoBackendKIL;
import org.kframework.backend.java.indexing.IndexingTable;
import org.kframework.backend.java.kil.CellLabel;
import org.kframework.backend.java.kil.ConstrainedTerm;
import org.kframework.backend.java.kil.Definition;
import org.kframework.backend.java.kil.GlobalContext;
import org.kframework.backend.java.kil.KLabelConstant;
import org.kframework.definition.Rule;
import org.kframework.backend.java.kil.Term;
import org.kframework.backend.java.kil.TermContext;
import org.kframework.backend.java.kil.Variable;
import org.kframework.backend.java.util.JavaKRunState;
import org.kframework.definition.Module;
import org.kframework.kompile.KompileOptions;
import org.kframework.kore.K;
import org.kframework.kore.KVariable;
import org.kframework.krun.KRunOptions;
import org.kframework.krun.api.KRunState;
import org.kframework.krun.api.SearchType;
import org.kframework.krun.api.io.FileSystem;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.inject.Builtins;
import org.kframework.utils.inject.DefinitionScoped;
import org.kframework.utils.inject.RequestScoped;
import org.kframework.utils.options.SMTOptions;
import scala.Tuple2;
import scala.collection.JavaConversions;

import java.lang.invoke.MethodHandle;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

/**
 * Created by dwightguth on 5/6/15.
 */
@RequestScoped
public class InitializeRewriter implements Function<Module, Rewriter> {

    private final FileSystem fs;
    private final JavaExecutionOptions javaOptions;
    private final GlobalOptions globalOptions;
    private final KExceptionManager kem;
    private final SMTOptions smtOptions;
    private final Map<String, Provider<MethodHandle>> hookProvider;
    private final KompileOptions kompileOptions;
    private final KRunOptions krunOptions;
    private final FileUtil files;
    private final InitializeDefinition initializeDefinition;
    private static int NEGATIVE_VALUE = -1;

    @Inject
    public InitializeRewriter(
            FileSystem fs,
            JavaExecutionOptions javaOptions,
            GlobalOptions globalOptions,
            KExceptionManager kem,
            SMTOptions smtOptions,
            @Builtins Map<String, Provider<MethodHandle>> hookProvider,
            KompileOptions kompileOptions,
            KRunOptions krunOptions,
            FileUtil files,
            InitializeDefinition initializeDefinition) {
        this.fs = fs;
        this.javaOptions = javaOptions;
        this.globalOptions = globalOptions;
        this.kem = kem;
        this.smtOptions = smtOptions;
        this.hookProvider = hookProvider;
        this.kompileOptions = kompileOptions;
        this.krunOptions = krunOptions;
        this.files = files;
        this.initializeDefinition = initializeDefinition;
    }

    public static Definition expandAndEvaluate(GlobalContext globalContext, KExceptionManager kem) {
        Definition expandedDefinition = new MacroExpander(TermContext.of(globalContext), kem).processDefinition();
        globalContext.setDefinition(expandedDefinition);

        Definition evaluatedDefinition = evaluateDefinition(globalContext);
        globalContext.setDefinition(evaluatedDefinition);
        return evaluatedDefinition;
    }

    /**
     * Partially evaluate the right-hand side and the conditions for each rule.
     */
    private static Definition evaluateDefinition(GlobalContext globalContext) {
        Definition definition = globalContext.getDefinition();
        /* replace the unevaluated rules defining functions with their partially evaluated counterparts */
        ArrayList<org.kframework.backend.java.kil.Rule> partiallyEvaluatedRules = new ArrayList<>();
        /* iterate until a fixpoint is reached, because the evaluation with functions uses Term#substituteAndEvalaute */
        while (true) {
            boolean change = false;

            partiallyEvaluatedRules.clear();
            for (org.kframework.backend.java.kil.Rule rule : Iterables.concat(definition.functionRules().values(),
                    definition.anywhereRules().values())) {
                org.kframework.backend.java.kil.Rule freshRule = rule.getFreshRule(TermContext.of(globalContext));
                org.kframework.backend.java.kil.Rule evaluatedRule = evaluateRule(freshRule, globalContext);
                partiallyEvaluatedRules.add(evaluatedRule);

                if (!evaluatedRule.equals(freshRule)) {
                    change = true;
                }
            }

            if (!change) {
                break;
            }

            definition.functionRules().clear();
            definition.anywhereRules().clear();
            definition.addRuleCollection(partiallyEvaluatedRules);
        }

        /* replace the unevaluated rules and macros with their partially evaluated counterparts */
        partiallyEvaluatedRules.clear();
        Iterable<org.kframework.backend.java.kil.Rule> rules = Iterables.concat(
                definition.rules(),
                definition.macros(),
                definition.patternRules().values(),
                definition.patternFoldingRules());
        for (org.kframework.backend.java.kil.Rule rule : rules) {
            partiallyEvaluatedRules.add(evaluateRule(rule, globalContext));
        }
        definition.rules().clear();
        definition.macros().clear();
        definition.patternRules().clear();
        definition.patternFoldingRules().clear();
        definition.addRuleCollection(partiallyEvaluatedRules);

        return definition;
    }

    /**
     * Partially evaluate the right-hand side and the conditions of a specified rule.
     */
    static org.kframework.backend.java.kil.Rule evaluateRule(org.kframework.backend.java.kil.Rule rule, GlobalContext globalContext) {
        try {
            TermContext termContext = TermContext.of(globalContext);
            // TODO(AndreiS): some evaluation is required in the LHS as well
            // TODO(YilongL): cannot simply uncomment the following code because it
            // may evaluate the LHS using the rule itself
            //Term leftHandSide = rule.leftHandSide().evaluate(termContext);

            org.kframework.backend.java.kil.Rule origRule = rule;
            Term rightHandSide = rule.rightHandSide().evaluate(termContext);
            List<Term> requires = new ArrayList<>();
            for (Term term : rule.requires()) {
                requires.add(term.evaluate(termContext));
            }
            List<Term> ensures = new ArrayList<>();
            for (Term term : rule.ensures()) {
                ensures.add(term.evaluate(termContext));
            }
            ConjunctiveFormula lookups = ConjunctiveFormula.of(termContext);
            for (Equality equality : rule.lookups().equalities()) {
                lookups = lookups.add(
                        equality.leftHandSide().evaluate(termContext),
                        equality.rightHandSide().evaluate(termContext));
            }

            Map<CellLabel, Term> rhsOfWriteCell = null;
            if (rule.isCompiledForFastRewriting()) {
                rhsOfWriteCell = new HashMap<>();
                for (Map.Entry<CellLabel, Term> entry : rule.rhsOfWriteCell().entrySet()) {
                    rhsOfWriteCell.put(entry.getKey(), entry.getValue().evaluate(termContext));
                }
            }

            org.kframework.backend.java.kil.Rule newRule = new org.kframework.backend.java.kil.Rule(
                    rule.label(),
                    rule.leftHandSide(),
                    rightHandSide,
                    requires,
                    ensures,
                    rule.freshConstants(),
                    rule.freshVariables(),
                    lookups,
                    rule.isCompiledForFastRewriting(),
                    rule.lhsOfReadCell(),
                    rhsOfWriteCell,
                    rule.cellsToCopy(),
                    rule.matchingInstructions(),
                    rule,
                    termContext);
            return newRule.equals(rule) ? origRule : newRule;
        } catch (KEMException e) {
            e.exception.addTraceFrame("while compiling rule at location " + rule.getSource() + rule.getLocation());
            throw e;
        }
    }

    public static Term expandAndEvaluate(GlobalContext globalContext, KExceptionManager kem, Term term) {
        term = new MacroExpander(TermContext.of(globalContext), kem).processTerm((Term) term);
        term = term.evaluate(TermContext.of(globalContext));
        return term;
    }

    @Override
    public synchronized Rewriter apply(Module module) {
        GlobalContext initializingContext = new GlobalContext(fs, javaOptions, globalOptions, krunOptions, kem, smtOptions, hookProvider, files, Stage.INITIALIZING);
        GlobalContext rewritingContext = new GlobalContext(fs, javaOptions, globalOptions, krunOptions, kem, smtOptions, hookProvider, files, Stage.REWRITING);
        Definition evaluatedDef = initializeDefinition.invoke(module, kem, initializingContext);
        rewritingContext.setDefinition(evaluatedDef);

        return new SymbolicRewriterGlue(evaluatedDef, kompileOptions, javaOptions, rewritingContext, kem);
    }

    public static class SymbolicRewriterGlue implements Rewriter {

        private final SymbolicRewriter rewriter;
        public final GlobalContext rewritingContext;
        private final KExceptionManager kem;

        public SymbolicRewriterGlue(Definition definition, KompileOptions kompileOptions, JavaExecutionOptions javaOptions, GlobalContext rewritingContext, KExceptionManager kem) {
            this.rewriter = new SymbolicRewriter(definition, kompileOptions, javaOptions, new KRunState.Counter());
            this.rewritingContext = rewritingContext;
            this.kem = kem;
        }

        @Override
        public RewriterResult execute(K k, Optional<Integer> depth) {
            KOREtoBackendKIL converter = new KOREtoBackendKIL(TermContext.of(rewritingContext));
            Term backendKil = expandAndEvaluate(rewritingContext, kem, converter.convert(k));
            JavaKRunState result = (JavaKRunState) rewriter.rewrite(new ConstrainedTerm(backendKil, TermContext.of(rewritingContext, backendKil, BigInteger.ZERO)), rewritingContext.getDefinition().context(), depth.orElse(-1), false);
            return new RewriterResult(result.getStepsTaken(), result.getJavaKilTerm());
        }

        @Override
        public List<Map<KVariable, K>> match(K k, org.kframework.definition.Rule rule) {
            throw new UnsupportedOperationException();
        }

        @Override
        public List<? extends Map<? extends KVariable, ? extends K>> search(K initialConfiguration, Optional<Integer> depth, Optional<Integer> bound, Rule pattern) {
            KOREtoBackendKIL converter = new KOREtoBackendKIL(TermContext.of(rewritingContext));
            Term javaTerm = expandAndEvaluate(rewritingContext, kem, converter.convert(initialConfiguration));
            org.kframework.backend.java.kil.Rule javaPattern = converter.convert(Optional.empty(), pattern);
            List<Substitution<Variable, Term>> searchResults;
            searchResults = rewriter.search(javaTerm, javaPattern, bound.orElse(NEGATIVE_VALUE), depth.orElse(NEGATIVE_VALUE),
                    SearchType.STAR, TermContext.of(rewritingContext), false);
            return searchResults;
        }


        public Tuple2<K, List<Map<KVariable, K>>> executeAndMatch(K k, Optional<Integer> depth, Rule rule) {
            K res = execute(k, depth).k();
            return Tuple2.apply(res, match(res, rule));
        }

    }


    @DefinitionScoped
    public static class InitializeDefinition {

        private final Map<Module, Definition> cache = new LinkedHashMap<Module, Definition>() {
            @Override
            protected boolean removeEldestEntry(Map.Entry<Module, Definition> eldest) {
                return this.size() > 20;
            }
        };

        public Definition invoke(Module module, KExceptionManager kem, GlobalContext initializingContext) {
            if (cache.containsKey(module)) {
                return cache.get(module);
            }
            Definition definition = new Definition(module, kem);

            TermContext termContext = TermContext.of(initializingContext);
            termContext.global().setDefinition(definition);

            JavaConversions.setAsJavaSet(module.attributesFor().keySet()).stream()
                    .map(l -> KLabelConstant.of(l.name(), definition))
                    .forEach(definition::addKLabel);
            definition.addKoreRules(module, termContext);

            Definition evaluatedDef = expandAndEvaluate(termContext.global(), kem);

            evaluatedDef.setIndex(new IndexingTable(() -> evaluatedDef, new IndexingTable.Data()));
            cache.put(module, evaluatedDef);
            return evaluatedDef;
        }
    }
}
