// Copyright (c) 2015 K Team. All Rights Reserved.
package org.kframework.backend.java.symbolic;

import com.google.inject.Inject;
import com.google.inject.Provider;
import org.kframework.Rewriter;
import org.kframework.backend.java.compile.KOREtoBackendKIL;
import org.kframework.backend.java.indexing.IndexingTable;
import org.kframework.backend.java.kil.ConstrainedTerm;
import org.kframework.backend.java.kil.Definition;
import org.kframework.backend.java.kil.GlobalContext;
import org.kframework.backend.java.kil.KLabelConstant;
import org.kframework.backend.java.kil.Term;
import org.kframework.backend.java.kil.TermContext;
import org.kframework.backend.java.kil.Variable;
import org.kframework.backend.java.util.JavaKRunState;
import org.kframework.definition.Module;
import org.kframework.definition.Rule;
import org.kframework.kil.Attribute;
import org.kframework.kompile.KompileOptions;
import org.kframework.kore.K;
import org.kframework.kore.KVariable;
import org.kframework.krun.KRunOptions;
import org.kframework.krun.api.KRunState;
import org.kframework.krun.api.io.FileSystem;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.inject.Builtins;
import org.kframework.utils.inject.DefinitionScoped;
import org.kframework.utils.inject.RequestScoped;
import org.kframework.utils.options.SMTOptions;
import scala.collection.JavaConversions;

import java.lang.invoke.MethodHandle;
import java.math.BigInteger;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
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

    @Override
    public synchronized Rewriter apply(Module module) {
        GlobalContext initializingContext = new GlobalContext(fs, javaOptions, globalOptions, krunOptions, kem, smtOptions, hookProvider, files, Stage.INITIALIZING);
        GlobalContext rewritingContext = new GlobalContext(fs, javaOptions, globalOptions, krunOptions, kem, smtOptions, hookProvider, files, Stage.REWRITING);
        Definition evaluatedDef = initializeDefinition.invoke(module, kem, initializingContext);
        rewritingContext.setDefinition(evaluatedDef);

        return new SymbolicRewriterGlue(evaluatedDef, kompileOptions, javaOptions, rewritingContext, kem, module);
    }

    public static class SymbolicRewriterGlue implements Rewriter {

        private final SymbolicRewriter rewriter;
        private final GlobalContext rewritingContext;
        private final KExceptionManager kem;
        private final Module module;

        public SymbolicRewriterGlue(Definition definition, KompileOptions kompileOptions, JavaExecutionOptions javaOptions, GlobalContext rewritingContext, KExceptionManager kem, Module module) {
            this.module = module;
            this.rewriter = new SymbolicRewriter(definition,  kompileOptions, javaOptions, new KRunState.Counter());
            this.rewritingContext = rewritingContext;
            this.kem = kem;
        }

        @Override
        public K convert(K k) {
            TermContext tc = TermContext.of(rewritingContext);
            KOREtoBackendKIL converter = new KOREtoBackendKIL(tc);
            return converter.convert(k, tc, kem);
        }

        @Override
        public List<? extends Map<? extends KVariable, ? extends K>> match(K k, boolean trace, Rule rule) {
            TermContext tc = TermContext.of(rewritingContext);
            KOREtoBackendKIL converter = new KOREtoBackendKIL(tc);
            Term backendKil = converter.convert(k, tc, kem);
            tc = TermContext.of(rewritingContext, backendKil, BigInteger.ZERO);
            org.kframework.backend.java.kil.Rule backendRule = converter.convert(module, tc, rule, kem);
            if (trace) {
                RuleAuditing.setAuditingRule(backendRule);
                RuleAuditing.beginAudit();
            }
            try {
                List<? extends Map<? extends KVariable, ? extends K>> res;
                if (rule.att().contains(Attribute.COMMUTATIVE_KEY) || rule.att().contains(Attribute.ASSOCIATIVE_KEY)) {
                    res = PatternMatcher.match(
                            backendKil,
                            backendRule,
                            tc);
                } else {
                    Map<Variable, Term> res2 = NonACPatternMatcher.match(backendKil, backendRule, tc);
                    if (res2 == null)
                        res = Collections.emptyList();
                    else {
                        res = Collections.singletonList(res2);
                    }
                }
                if (res.size() > 0) {
                    RuleAuditing.succeed(backendRule);
                } else if (RuleAuditing.isAudit()) {
                    RuleAuditing.fail();
                }
                return res;
            } finally {
                RuleAuditing.endAudit();
                RuleAuditing.clearAuditingRule();
            }
        }

        @Override
        public K substitute(Map<? extends KVariable, ? extends K> substitution, Rule rule) {
            Map<Variable, Term> backendSubst = new HashMap<>();
            TermContext tc = TermContext.of(rewritingContext);
            KOREtoBackendKIL converter = new KOREtoBackendKIL(tc);
            for (Map.Entry<? extends KVariable, ? extends K> entry : substitution.entrySet()) {
                backendSubst.put((Variable)converter.convert(entry.getKey(), tc, kem), converter.convert(entry.getValue(), tc, kem));
            }
            org.kframework.backend.java.kil.Rule backendRule = converter.convert(module, tc, rule, kem);
            return backendRule.rightHandSide().substituteAndEvaluate(backendSubst, tc);
        }

        @Override
        public K execute(K k) {
            TermContext tc = TermContext.of(rewritingContext);
            KOREtoBackendKIL converter = new KOREtoBackendKIL(tc);
            Term backendKil = converter.convert(k, tc, kem);
            JavaKRunState result = (JavaKRunState) rewriter.rewrite(new ConstrainedTerm(backendKil, TermContext.of(rewritingContext, backendKil, BigInteger.ZERO)), rewritingContext.getDefinition().context(), -1, false);
            return result.getJavaKilTerm();
        }

        @Override
        public List<? extends Rule> rules() {
            return rewritingContext.getDefinition().rules();
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

            Definition evaluatedDef = KILtoBackendJavaKILTransformer.expandAndEvaluate(termContext.global(), kem);

            evaluatedDef.setIndex(new IndexingTable(() -> evaluatedDef, new IndexingTable.Data()));
            cache.put(module, evaluatedDef);
            return evaluatedDef;
        }
    }
}
