// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.backend.java.kil;

import java.io.Serializable;
import java.util.List;

import org.kframework.backend.java.symbolic.Matcher;
import org.kframework.backend.java.symbolic.Transformer;
import org.kframework.backend.java.symbolic.Unifier;
import org.kframework.backend.java.symbolic.Visitor;
import org.kframework.kil.ASTNode;
import org.pcollections.ConsPStack;
import org.pcollections.PStack;

import com.google.common.collect.HashMultiset;
import com.google.common.collect.ImmutableMultiset;
import com.google.common.collect.Lists;
import com.google.common.collect.Multiset;


/**
 * Represents a list of KItem, or {@link KItem} in the Java backend. Or in the
 * usual syntax of K, it can be defined as the following:
 * <p>
 * <blockquote>
 *
 * <pre>
 * syntax K ::= List{KItem}{"~>"}
 * </pre>
 *
 * </blockquote>
 * <p>
 *
 * @author AndreiS
 */
@SuppressWarnings("serial")
public class KSequence extends KCollection {

    private static final String SEPARATOR_NAME = " ~> ";
    private static final String IDENTITY_NAME = "." + Kind.K;
    public static final KSequence EMPTY = new KSequence(ConsPStack.<Term>empty(), null, ImmutableMultiset.<Variable>of());

    private transient final PStack<Term> contents;

    /**
     * Set of variables of sort K in contents.
     */
    private final ImmutableMultiset<Variable> kSequenceVariables;

    /**
     * Builds a single-element KSequence based on the given term. This method is
     * necessary in addition to the {@code Builder} because the {@code Builder}
     * will canonicalize its result and, thus, simply return the given term.
     */
    public static KSequence singleton(Term term) {
        assert term.kind().equals(Kind.KITEM);
        return new KSequence(ConsPStack.singleton(term), null, ImmutableMultiset.<Variable>of());
    }

    private KSequence(PStack<Term> contents, Variable frame, ImmutableMultiset<Variable> kSequenceVariables) {
        super(frame, Kind.K);
        this.contents = contents;
        this.kSequenceVariables = kSequenceVariables;
    }

    @Override
    public Term fragment(int fromIndex) {
        if (fromIndex == contents.size()) {
            return hasFrame() ? frame : EMPTY;
        } else {
            if (kSequenceVariables.isEmpty()) {
                return new KSequence(contents.subList(fromIndex), frame, kSequenceVariables);
            } else {
                /* YilongL: this case should never happen in practice because
                 * this method should only be called by KSequence on the LHS */
                Multiset<Variable> multiset = HashMultiset.create(kSequenceVariables);
                multiset.removeAll(contents.subList(0, fromIndex));
                return new KSequence(contents.subList(fromIndex), frame, ImmutableMultiset.copyOf(multiset));
            }
        }
    }

    @Override
    public List<Term> getContents() {
        return contents;
    }

    @Override
    public final boolean isConcreteCollection() {
        return !hasFrame() && kSequenceVariables.isEmpty();
    }

    @Override
    public Sort sort() {
        return !hasFrame() && concreteSize() == 1 ? contents.get(0).sort() : Sort.KSEQUENCE;
    }

    @Override
    public String getSeparatorName() {
        return KSequence.SEPARATOR_NAME;
    }

    @Override
    public String getIdentityName() {
        return KSequence.IDENTITY_NAME;
    }

    @Override
    public boolean equals(Object object) {
        if (this == object) {
            return true;
        }

        if (!(object instanceof KSequence)) {
            return false;
        }

        KSequence kSequence = (KSequence) object;
        return (frame == null ? kSequence.frame == null : frame
                .equals(kSequence.frame))
                && contents.equals(kSequence.contents);
    }

    @Override
    public void accept(Unifier unifier, Term pattern) {
        unifier.unify(this, pattern);
    }

    @Override
    public void accept(Matcher matcher, Term pattern) {
        matcher.match(this, pattern);
    }

    @Override
    public void accept(Visitor visitor) {
        visitor.visit(this);
    }

    @Override
    public ASTNode accept(Transformer transformer) {
        return transformer.transform(this);
    }

    public static Builder builder() {
        return new Builder();
    }

    // TODO(YilongL): add JUnit tests!
    public static class Builder {

        private final List<Term> terms = Lists.newArrayListWithExpectedSize(32);

        private final ImmutableMultiset.Builder<Variable> variablesBuilder = ImmutableMultiset.builder();

        // TODO(YilongL): rename it to prepend and change the implementation accordingly
        public void concatenate(Term term) {
            if (term instanceof KSequence) {
                if (!term.equals(EMPTY)) {
                    terms.add(term);
                }
            } else if (term instanceof Variable) {
                assert term.sort().equals(Sort.KSEQUENCE) || term.kind().equals(Kind.KITEM);
                terms.add(term);
            } else if (term.kind().equals(Kind.KITEM)) {
                terms.add(term);
            } else if (term instanceof KItemProjection) {
                // TODO(AndreiS): fix KItem projection
                terms.add(term);
            } else {
                assert false : "unexpected concatenated term" + term;
            }
        }

        /**
         * Returns a newly-created canonicalized KSequence based on the contents of the builder.
         */
        public Term build() {
            if (terms.isEmpty()) {
                return EMPTY;
            } else if (terms.size() == 1) {
                return terms.get(0);
            } else {
                Variable frame = null;
                KSequence ksequence = null; // reusable KSequence
                int index = terms.size() - 1;

                /* compute ksequence and frame */
                Term term1 = terms.get(terms.size() - 1);
                Term term2 = terms.get(terms.size() - 2);
                if (term1 instanceof Variable && term1.sort().equals(Sort.KSEQUENCE)) {
                    index--;
                    frame = (Variable) term1;
                    if (term2 instanceof KSequence && !((KSequence) term2).hasFrame()) {
                        index--;
                        ksequence = (KSequence) term2;
                    }
                } else if (term1 instanceof KSequence) {
                    index--;
                    ksequence = (KSequence) term1;
                }

                /* build contents */
                PStack<Term> contents = ConsPStack.empty();
                if (ksequence != null) {
                    contents = ksequence.contents;
                    if (ksequence.hasFrame()) {
                        assert frame == null;
                        frame = ksequence.frame;
                    }
                }

                for (int i = index; i >= 0; i--) {
                    Term term = terms.get(i);
                    if (term instanceof KSequence) {
                        KSequence kseq = (KSequence) term;
                        contents = kseq.hasFrame() ? contents.plus(kseq.frame) : contents;
                        contents = kseq.contents.size() <= 1 ?
                                    contents.plusAll(kseq.contents) :
                                    contents.plusAll(Lists.reverse(Lists.newArrayList(kseq.contents)));
                        variablesBuilder.addAll(kseq.kSequenceVariables);
                    } else {
                        contents = contents.plus(term);
                        if (term instanceof Variable && term.sort().equals(Sort.KSEQUENCE)) {
                            variablesBuilder.add((Variable) term);
                        }
                    }
                }
                return new KSequence(contents, frame, variablesBuilder.build());
            }
        }
    }

    /**
     * When this {@code KSequence} is being serialized, serialize its proxy
     * class instead.
     *
     * @return the serialization proxy of this {@code KSequence}
     */
    private Object writeReplace() {
        return new SerializationProxy(this);
    }

    private static class SerializationProxy implements Serializable {

        private List<Term> terms;
        private Variable frame;
        private ImmutableMultiset<Variable> variables;

        private SerializationProxy(KSequence kSequence) {
            /* converts the non-serializable PStack to normal list */
            this.terms = Lists.newArrayList(kSequence.contents);
            this.frame = kSequence.frame;
            this.variables = kSequence.kSequenceVariables;
        }

        private Object readResolve() {
            PStack<Term> contents = ConsPStack.empty();
            for (Term term : Lists.reverse(terms)) {
                contents = contents.plus(term);
            }
            return new KSequence(contents, frame, variables);
        }
    }

}
