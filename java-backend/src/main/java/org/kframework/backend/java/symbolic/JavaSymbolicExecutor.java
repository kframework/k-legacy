// Copyright (c) 2013-2015 K Team. All Rights Reserved.
package org.kframework.backend.java.symbolic;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import edu.uci.ics.jung.graph.util.Pair;
import org.kframework.backend.java.kil.ConstrainedExecutionGraph;
import org.kframework.backend.java.kil.ConstrainedRewriteRelation;
import org.kframework.backend.java.kil.ConstrainedTerm;
import org.kframework.backend.java.kil.Definition;
import org.kframework.backend.java.kil.GlobalContext;
import org.kframework.backend.java.kil.Rule;
import org.kframework.backend.java.kil.Term;
import org.kframework.backend.java.kil.TermContext;
import org.kframework.backend.java.kil.Transition;
import org.kframework.backend.java.kil.Variable;
import org.kframework.compile.utils.RuleCompilerSteps;
import org.kframework.kil.loader.Context;
import org.kframework.krun.KRunExecutionException;
import org.kframework.krun.SubstitutionFilter;
import org.kframework.krun.api.KRunGraph;
import org.kframework.krun.api.KRunState;
import org.kframework.krun.api.RewriteRelation;
import org.kframework.krun.api.SearchResult;
import org.kframework.krun.api.SearchResults;
import org.kframework.krun.api.SearchType;
import org.kframework.krun.tools.Executor;
import com.google.inject.Inject;
import com.google.inject.Provider;

public class JavaSymbolicExecutor implements Executor {

    private final JavaExecutionOptions javaOptions;
    private final KILtoBackendJavaKILTransformer kilTransformer;
    private final GlobalContext globalContext;
    private final Provider<SymbolicRewriter> symbolicRewriter;
    private final Provider<PatternMatchRewriter> patternMatchRewriter;
    private final KILtoBackendJavaKILTransformer transformer;
    private final Context context;
    private final KRunState.Counter counter;
    private final static int unboundSteps = -1;

    @Inject
    JavaSymbolicExecutor(
            Context context,
            JavaExecutionOptions javaOptions,
            KILtoBackendJavaKILTransformer kilTransformer,
            GlobalContext globalContext,
            Provider<SymbolicRewriter> symbolicRewriter,
            Provider<PatternMatchRewriter> patternMatchRewriter,
            KILtoBackendJavaKILTransformer transformer,
            Definition definition,
            KRunState.Counter counter) {
        this.context = context;
        this.javaOptions = javaOptions;
        this.kilTransformer = kilTransformer;
        this.globalContext = globalContext;
        this.symbolicRewriter = symbolicRewriter;
        this.patternMatchRewriter = patternMatchRewriter;
        this.transformer = transformer;
        globalContext.setDefinition(definition);
        this.counter = counter;
    }

    @Override
    public RewriteRelation run(org.kframework.kil.Term cfg, boolean computeGraph) throws KRunExecutionException {
        return step(cfg, unboundSteps, computeGraph);
    }

    @Override
    public SearchResults search(
            Integer bound,
            Integer depth,
            SearchType searchType,
            org.kframework.kil.Rule pattern,
            org.kframework.kil.Term cfg,
            RuleCompilerSteps compilationInfo) throws KRunExecutionException {

        List<Rule> claims = Collections.emptyList();
        if (bound == null) {
            bound = -1;
        }
        if (depth == null) {
            depth = -1;
        }

        // The pattern needs to be a rewrite in order for the transformer to be
        // able to handle it, so we need to give it a right-hand-side.
        org.kframework.kil.Cell c = new org.kframework.kil.Cell();
        c.setLabel("generatedTop");
        c.setContents(new org.kframework.kil.Bag());
        pattern.setBody(new org.kframework.kil.Rewrite(pattern.getBody(), c, context));
        Rule patternRule = transformer.transformAndEval(pattern);

        List<SearchResult> searchResults = new ArrayList<SearchResult>();
        List<Map<Variable,Term>> hits;
        Term initialTerm = kilTransformer.transformAndEval(cfg);
        Term targetTerm = null;
        TermContext termContext = TermContext.of(globalContext);
        if (javaOptions.patternMatching) {
            hits = getPatternMatchRewriter().search(initialTerm, targetTerm, claims,
                    patternRule, bound, depth, searchType, termContext);
        } else {
            hits = getSymbolicRewriter().search(initialTerm, targetTerm, claims,
                    patternRule, bound, depth, searchType, termContext);
        }

        for (Map<Variable,Term> map : hits) {
            // Construct substitution map from the search results
            Map<String, org.kframework.kil.Term> substitutionMap =
                    new HashMap<String, org.kframework.kil.Term>();
            for (Variable var : map.keySet()) {
                org.kframework.kil.Term kilTerm =
                        (org.kframework.kil.Term) map.get(var).accept(
                                new BackendJavaKILtoKILTransformer(context));
                substitutionMap.put(var.name(), kilTerm);
            }

            // Apply the substitution to the pattern
            org.kframework.kil.Term rawResult =
                    (org.kframework.kil.Term) new SubstitutionFilter(substitutionMap, context)
                        .visitNode(pattern.getBody());

            searchResults.add(new SearchResult(
                    new KRunState(rawResult, counter),
                    substitutionMap,
                    compilationInfo));
        }

        SearchResults retval = new SearchResults(
                searchResults,
                null);

        return retval;
    }


    private KRunGraph genericGraphTransformer(ConstrainedExecutionGraph constrainedGraph) {
        KRunGraph returnGraph = new KRunGraph();
        for (Transition javaTransition : constrainedGraph.getEdges()) {
            Map<org.kframework.kil.Variable, org.kframework.kil.Term> genericSubstitution = new HashMap<>();
            Map<Variable, Term> javaSubstitution = javaTransition.getSubstitution();
            /* Process Substitution */
            for (Variable key : javaSubstitution.keySet()) {
                org.kframework.kil.Variable genericKey = (org.kframework.kil.Variable) key.accept(
                        new BackendJavaKILtoKILTransformer(context));
                org.kframework.kil.Term genericValue = (org.kframework.kil.Term) javaSubstitution.get(key).accept(
                        new BackendJavaKILtoKILTransformer(context));
                genericSubstitution.put(genericKey, genericValue);
            }
            /* Process Rule */
            //Todo: Transformer for rule has not yet been implemented, hence this line will fail
            org.kframework.kil.Rule rule = (org.kframework.kil.Rule) javaTransition.getRule().accept(
                    new BackendJavaKILtoKILTransformer(context));
            org.kframework.krun.api.Transition genericTransition = new org.kframework.krun.api.Transition(
                    org.kframework.krun.api.Transition.TransitionType.RULE, "", rule, "", genericSubstitution);

            /* Process nodes connecting transition */
            Pair<ConstrainedTerm> nodes = constrainedGraph.getEndpoints(javaTransition);
            org.kframework.kil.Term node1 = (org.kframework.kil.Term) nodes.getFirst().term().accept(
                    new BackendJavaKILtoKILTransformer(context)
            );
            KRunState state1 = new KRunState(node1, counter);


            org.kframework.kil.Term node2 = (org.kframework.kil.Term) nodes.getSecond().term().accept(
                    new BackendJavaKILtoKILTransformer(context)
            );
            KRunState state2 = new KRunState(node2, counter);
            returnGraph.addVertex(state1);
            returnGraph.addVertex(state2);
            returnGraph.addEdge(genericTransition, state1, state2);

        }
        return returnGraph;
    }

    private RewriteRelation genericKilTransformer(ConstrainedRewriteRelation constrainedRewriteRelation) {
        /* processing the final Term */
        org.kframework.kil.Term finalTerm = (org.kframework.kil.Term) constrainedRewriteRelation.getFinalTerm().term().accept(
                new BackendJavaKILtoKILTransformer(context)
        );
        RewriteRelation returnRelation = new RewriteRelation(new KRunState(finalTerm, counter));
        KRunGraph executionGraph = null;
        /* Processing the execution trace, if existent */
        if(constrainedRewriteRelation.getConstrainedExecutionGraph().isPresent()) {
            executionGraph = genericGraphTransformer(constrainedRewriteRelation.
                    getConstrainedExecutionGraph().get());
        }
        returnRelation.setExecutionGraph(executionGraph);
        return returnRelation;
    }

    private ConstrainedRewriteRelation internalTraceStep(org.kframework.kil.Term cfg, int steps, boolean computeGraph)
            throws KRunExecutionException{
        Term term = kilTransformer.transformAndEval(cfg);
        TermContext termContext = TermContext.of(globalContext);
        if (javaOptions.patternMatching) {
            if (computeGraph) {
                throw new KRunExecutionException("compute Graph with pattern matching not yet implemented!");
            }
            ConstrainedTerm rewriteResult = new ConstrainedTerm(getPatternMatchRewriter().rewrite(term, steps, termContext), termContext);
            return new ConstrainedRewriteRelation(rewriteResult, null);
        }
        SymbolicConstraint constraint = new SymbolicConstraint(termContext);
        ConstrainedTerm constrainedTerm = new ConstrainedTerm(term, constraint);
        return getSymbolicRewriter().traceRewrite(constrainedTerm, steps, computeGraph);

    }

    @Override
    public RewriteRelation step(org.kframework.kil.Term cfg, int steps, boolean computeGraph) throws KRunExecutionException{
        ConstrainedRewriteRelation traceResult = internalTraceStep(cfg, steps, computeGraph);
        return genericKilTransformer(traceResult);
    }


    public SymbolicRewriter getSymbolicRewriter() {
        return symbolicRewriter.get();
    }

    private PatternMatchRewriter getPatternMatchRewriter() {
        return patternMatchRewriter.get();
    }
}
