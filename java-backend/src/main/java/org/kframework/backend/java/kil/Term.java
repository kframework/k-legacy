// Copyright (c) 2013-2015 K Team. All Rights Reserved.
package org.kframework.backend.java.kil;

import com.google.common.collect.ImmutableList;
import org.kframework.attributes.Att;
import org.kframework.attributes.Source;
import org.kframework.attributes.Location;
import org.kframework.backend.java.indexing.IndexingPair;
import org.kframework.backend.java.rewritemachine.GenerateRHSInstructions;
import org.kframework.backend.java.rewritemachine.RHSInstruction;
import org.kframework.backend.java.symbolic.BottomUpVisitor;
import org.kframework.backend.java.symbolic.CopyOnShareSubstAndEvalTransformer;
import org.kframework.backend.java.symbolic.Evaluator;
import org.kframework.backend.java.symbolic.SubstituteAndEvaluateTransformer;
import org.kframework.backend.java.symbolic.Transformable;
import org.kframework.backend.java.util.Profiler;
import org.kframework.backend.java.util.Utils;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.Set;


/**
 * A K term in the internal representation of the Java Rewrite Engine.
 *
 * @author AndreiS
 */
public abstract class Term extends JavaSymbolicObject implements Transformable, Comparable<Term>,
        org.kframework.kore.K {

    protected final Kind kind;
    // protected final boolean normalized;

    private Boolean mutable = null;

    volatile transient ImmutableList<RHSInstruction> instructions = null;

    protected Term(Kind kind) {
        super();
        this.kind = kind;
    }

    protected Term(Kind kind, Source source, Location location) {
        super(source, location);
        this.kind = kind;
    }

    /**
     * Returns a {@link List} view of the indexing pairs from the {@code k}
     * cells of this {@code Term}.
     */
    public List<IndexingPair> getKCellIndexingPairs(final Definition definition) {
        final List<IndexingPair> indexingPairs = new ArrayList<>();
        for (Term content : getCellContentsByName(CellLabel.K)) {
            indexingPairs.add(IndexingPair.getKCellIndexingPair(content, definition));
        }
        return indexingPairs;
    }

    /**
     * Returns true if this term has exactly the sort returned by {@link #sort()},
     * and not any of its proper subsorts.
     *
     * @see #sort()
     */
    public abstract boolean isExactSort();

    /**
     * Returns {@code true} if a unification task between this term and another term cannot be
     * further decomposed into simpler unification tasks.
     */
    public abstract boolean isSymbolic();

    /**
     * Returns the kind of this term (Cell, CellCollection, KItem, K, KLabel, KList, or Map).
     */
    public Kind kind() {
        return kind;
    }

    public abstract Sort sort();

    /**
     * @return {@code true} if this term has {@code Cell} inside; otherwise,
     *         {@code false}
     */
    public final boolean isMutable() {
        if (mutable == null) {
            mutable = computeMutability();
        }
        return mutable;
    }

    /**
     * Checks if this term has {@code Cell} inside.
     */
    protected abstract boolean computeMutability();

    /**
     * Returns a new {@code Term} instance obtained from this term by evaluating
     * pending functions and predicates. <br>
     */
    public Term evaluate(TermContext context) {
        return Evaluator.evaluate(this, context);
    }

    /**
     * Returns a new {@code Term} instance obtained from this term by applying a binder insensitive substitution.
     */
    @Override
    public Term substitute(Map<Variable, ? extends Term> substitution, TermContext context) {
        return (Term) super.substitute(substitution, context);
    }

    /**
     * Returns a new {@code Term} instance obtained from this term by applying a binder-aware substitution.
     */
    @Override
    public Term substituteWithBinders(Map<Variable, ? extends Term> substitution, TermContext context) {
        return (Term) super.substituteWithBinders(substitution, context);
    }

    /**
     * Returns a new {@code Term} instance obtained from this term by applying
     * substitution and evaluating functions.
     */
    public Term substituteAndEvaluate(Map<Variable, ? extends Term> substitution, TermContext context) {
        Profiler.startTimer(Profiler.INSTRUCTIONS_UPDATE_TIMER);
        /*
        if (instructions == null) {
            Profiler.startTimer(Profiler.INSTRUCTIONS_UPDATE_TIMER);
            GenerateRHSInstructions visitor = new GenerateRHSInstructions(context);
            this.accept(visitor);
            instructions = visitor.getInstructions();
            Profiler.stopTimer(Profiler.INSTRUCTIONS_UPDATE_TIMER);
        }

        Deque<Term> stack = new ArrayDeque<>();
        for (RHSInstruction instruction : instructions) {
            switch (instruction.type()) {
            case PUSH:
                Term t = instruction.term();
                stack.push(t);
                break;
            case CONSTRUCT:
                RHSInstruction.Constructor constructor = instruction.constructor();
                switch (constructor.type()) {
                case BUILTIN_LIST:
                    BuiltinList.Builder builder = BuiltinList.builder(context);
                    for (int i = 0; i < constructor.size1(); i++) {
                        builder.addItem(stack.pop());
                    }
                    for (int i = 0; i < constructor.size2(); i++) {
                        builder.concatenate(stack.pop());
                    }
                    for (int i = 0; i < constructor.size3(); i++) {
                        builder.addItem(stack.pop());
                    }
                    stack.push(builder.build());
                    break;
                case BUILTIN_MAP:
                    BuiltinMap.Builder builder1 = BuiltinMap.builder(context);
                    for (int i = 0; i < constructor.size1(); i++) {
                        Term key = stack.pop();
                        Term value = stack.pop();
                        builder1.put(key, value);
                    }
                    for (int i = 0; i < constructor.size2(); i++) {
                        builder1.concatenate(stack.pop());
                    }
                    stack.push(builder1.build());
                    break;
                case BUILTIN_SET:
                    BuiltinSet.Builder builder2 = BuiltinSet.builder(context);
                    for (int i = 0; i < constructor.size1(); i++) {
                        builder2.add(stack.pop());
                    }
                    for (int i = 0; i < constructor.size2(); i++) {
                        builder2.concatenate(stack.pop());
                    }
                    stack.push(builder2.build());
                    break;
                case KITEM:
                    Term kLabel = stack.pop();
                    Term kList = stack.pop();
                    stack.push(KItem.of(kLabel, kList, context, constructor.getSource(), constructor.getLocation()));
                    break;
                case KITEM_PROJECTION:
                    stack.push(new KItemProjection(constructor.kind(), stack.pop()));
                    break;
                case KLABEL_FREEZER:
                    stack.push(new KLabelFreezer(stack.pop()));
                    break;
                case KLABEL_INJECTION:
                    stack.push(new KLabelInjection(stack.pop()));
                    break;
                case INJECTED_KLABEL:
                    stack.push(new InjectedKLabel(stack.pop()));
                    break;
                case KLIST:
                    KList.Builder builder3 = KList.builder();
                    for (int i = 0; i < constructor.size1(); i++) {
                        builder3.concatenate(stack.pop());
                    }
                    stack.push(builder3.build());
                    break;
                case KSEQUENCE:
                    KSequence.Builder builder4 = KSequence.builder();
                    for (int i = 0; i < constructor.size1(); i++) {
                        builder4.concatenate(stack.pop());
                    }
                    stack.push(builder4.build());
                    break;
                case CELL_COLLECTION:
                    CellCollection.Builder builder5 = CellCollection.builder(context.definition());
                    for (CellLabel cellLabel : constructor.cellLabels()) {
                        builder5.add(new CellCollection.Cell(cellLabel, stack.pop()));
                    }
                    for (int i = 0; i < constructor.size1(); i++) {
                        builder5.concatenate(stack.pop());
                    }
                    stack.push(builder5.build());
                    break;
                default:
                    throw new AssertionError("unreachable");
                }
                break;
            case SUBST:
                Variable variable = (Variable) instruction.term();
                Term term = substitution.get(variable);
                if (term == null) {
                    term = variable;
                }
                stack.push(term);
                break;
            case EVAL:
                KItem kItem = (KItem) stack.pop();
                stack.push(kItem.resolveFunctionAndAnywhere(true, context));
                break;
            case PROJECT:
                KItemProjection projection = (KItemProjection) stack.pop();
                stack.push(projection.evaluateProjection());
                break;
            }
        }
        assert stack.size() == 1;
        return stack.pop();
        */

        Term term = canSubstituteAndEvaluate(substitution) ?
                (Term) this.accept(new SubstituteAndEvaluateTransformer(substitution, context)) :
                this;
        Profiler.stopTimer(Profiler.INSTRUCTIONS_UPDATE_TIMER);
        return term;
    }

    /**
     * Similar to {@link Term#substituteAndEvaluate(Map, TermContext)} except
     * that this method will copy the terms used for substitution whenever
     * necessary in order to avoid undesired sharing of mutable terms.
     *
     * @param substitution
     *            the substitution map; TODO(YilongL): this may become a
     *            multi-map in the future when the pattern matching algorithm
     *            allows us to record multiple equal terms binding to a variable
     *            for the sake of maximizing term reuse
     * @param variablesToReuse
     *            a set of variables in the substitution whose binding terms can
     *            be reused to build the new term
     * @param context
     * @return a new term obtained by applying substitution
     */
    public Term copyOnShareSubstAndEval(
            Map<Variable, ? extends Term> substitution,
            Set<Variable> variablesToReuse, TermContext context) {
        if (!canSubstituteAndEvaluate(substitution)) {
            return this;
        }
        CopyOnShareSubstAndEvalTransformer transformer = new CopyOnShareSubstAndEvalTransformer(
                substitution, variablesToReuse, context);
        return (Term) this.accept(transformer);
    }

    /**
     * Similar to {@link Term#copyOnShareSubstAndEval(Map, Set, TermContext)}
     * except the empty reusable variable set.
     *
     * @see {@link Term#copyOnShareSubstAndEval(Map, Set, TermContext)}
     */
    public Term copyOnShareSubstAndEval(Map<Variable, ? extends Term> substitution, TermContext context) {
        return copyOnShareSubstAndEval(substitution, Collections.<Variable>emptySet(), context);
    }

    /**
     * Returns a list containing the contents of each occurrence of a cell with the given name.
     *
     * Warning: this is slow!
     * TODO(YilongL): improve performance when better indexing is available
     */
    public List<Term> getCellContentsByName(final CellLabel cellLabel) {
        final List<Term> contents = new ArrayList<>();
        accept(new BottomUpVisitor() {
            @Override
            public void visit(CellCollection cellCollection) {
                for (CellCollection.Cell cell : cellCollection.get(cellLabel)) {
                    contents.add(cell.content());
                }
                for (CellCollection.Cell cell : cellCollection.cells().values()) {
                    if (cell.content() instanceof CellCollection) {
                        visit((CellCollection) cell.content());
                    }
                }
            }
        });
        return contents;
    }

     /**
     * Returns a new {@code Term} instance obtained from this term by substituting variable with
     * term.
     */
    @Override
    public Term substituteWithBinders(Variable variable, Term term, TermContext context) {
        return (Term) super.substituteWithBinders(variable, term, context);
    }

    @Override
    public final int compareTo(Term o) {
        /* implement compareTo() in a way that the expensive toString() is
         * rarely called */
        if (hashCode() > o.hashCode()) {
            return 1;
        } else if (hashCode() < o.hashCode()) {
            return -1;
        } else if (equals(o)) {
            return 0;
        } else {
            /* Note: since the equality has been checked, it's okay that the
             * two different terms might have the same string representation */
            return toString().compareTo(o.toString());
        }
    }

    /**
     * Computes and caches the hashCode if it has not been computed yet.
     * Otherwise, simply returns the cached value.
     */
    @Override
    public final int hashCode() {
        int h = hashCode;
        if (h == Utils.NO_HASHCODE) {
            h = computeHash();
            h = h == 0 ? 1 : h;
            hashCode = h;
        }
        return h;
    }

    /**
     * (Re-)computes the hashCode of this {@code Term}.
     * @return the hash code
     */
    protected abstract int computeHash();

    @Override
    public abstract boolean equals(Object object);

    public Att att() {
        return Att.apply();
    }

    public Location location() { return getLocation(); }
    public Source source() { return getSource(); }
}
