// Copyright (c) 2013-2015 K Team. All Rights Reserved.
package org.kframework.backend.java.symbolic;

import com.google.common.collect.Lists;
import org.apache.commons.lang3.tuple.Pair;
import org.kframework.backend.java.kil.Bottom;
import org.kframework.backend.java.kil.BuiltinList;
import org.kframework.backend.java.kil.BuiltinMap;
import org.kframework.backend.java.kil.BuiltinSet;
import org.kframework.backend.java.kil.CellCollection;
import org.kframework.backend.java.kil.ConcreteCollectionVariable;
import org.kframework.backend.java.kil.Hole;
import org.kframework.backend.java.kil.InjectedKLabel;
import org.kframework.backend.java.kil.KCollection;
import org.kframework.backend.java.kil.KItem;
import org.kframework.backend.java.kil.KLabelConstant;
import org.kframework.backend.java.kil.KLabelInjection;
import org.kframework.backend.java.kil.KList;
import org.kframework.backend.java.kil.KSequence;
import org.kframework.backend.java.kil.Term;
import org.kframework.backend.java.kil.TermContext;
import org.kframework.backend.java.kil.Token;
import org.kframework.utils.errorsystem.KEMException;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;


/**
 * @author AndreiS
 */
public abstract class AbstractUnifier implements Unifier {
    /**
     * Left-hand side of a minimal equality causing this unification to fail.
     * Must be set if this unification fails.
     */
    private Term unificationFailureLeftHandSide;
    /**
     * Right-hand side of a minimal equality causing this unification to fail.
     * Must be set if this unification fails.
     */
    private Term unificationFailureRightHandSide;

    public Term unificationFailureLeftHandSide() {
        return unificationFailureLeftHandSide;
    }

    public Term unificationFailureRightHandSide() {
        return unificationFailureRightHandSide;
    }

    /**
     * Fails the unification task.
     */
    protected void fail(Term term, Term otherTerm) {
        unificationFailureLeftHandSide = term;
        unificationFailureRightHandSide = otherTerm;
        failed = true;
        if (RuleAuditing.isAuditBegun()) {
            System.err.println("matching/unification failure: " + term + " and " + otherTerm);
        }
    }

    private final Deque<Pair<Term, Term>> tasks = new ArrayDeque<>();

    protected boolean failed = false;

    //private boolean matchOnFunctionSymbol;
    //
    //private TermContext termContext;

    void addUnificationTask(Term term, Term otherTerm) {
        tasks.addFirst(Pair.of(term, otherTerm));
    }

    /**
     * Performs generic operations for the unification of two terms.
     * Term-specific operations are then delegated to the specific {@code unify}
     * method by overloading. That is to say, in general, the safe way to unify
     * any two terms is to invoke this generic {@code unify} method; do not
     * invoke the specialized ones directly unless you know exactly what you are
     * doing.
     */
    protected boolean unify() {
        while (!failed && !tasks.isEmpty()) {
            Pair<Term, Term> task = tasks.pop();
            Term term = task.getLeft();
            Term otherTerm = task.getRight();

            if (term.isGround() && otherTerm.isGround()
                    && term.isNormal() && otherTerm.isNormal()) {
                if (!term.equals(otherTerm)) {
                    fail(term, otherTerm);
                    break;
                }
            }

            if (term.kind().isComputational()) {
                assert otherTerm.kind().isComputational() : otherTerm;

                term = KCollection.upKind(term, otherTerm.kind());
                otherTerm = KCollection.upKind(otherTerm, term.kind());
            }

            assert term.kind() == otherTerm.kind();

            if (stop(term, otherTerm)) {
                continue;
            }

            if (term.hashCode() == otherTerm.hashCode() && term.equals(otherTerm)) {
                continue;
            }

            /* unify */
            if (term instanceof CellCollection && otherTerm instanceof CellCollection) {
                unify((CellCollection) term, (CellCollection) otherTerm);
            } else if (term instanceof KItem && otherTerm instanceof KItem) {
                unify((KItem) term, (KItem) otherTerm);
            } else if (term instanceof KLabelConstant && otherTerm instanceof KLabelConstant) {
                unify((KLabelConstant) term, (KLabelConstant) otherTerm);
            } else if (term instanceof KList && otherTerm instanceof KList) {
                unify((KList) term, (KList) otherTerm);
            } else if (term instanceof KSequence && otherTerm instanceof KSequence) {
                unify((KSequence) term, (KSequence) otherTerm);
            } else if (term instanceof Token && otherTerm instanceof Token) {
                unify((Token) term, (Token) otherTerm);
            } else if (term instanceof InjectedKLabel && otherTerm instanceof InjectedKLabel) {
                unify((InjectedKLabel) term, (InjectedKLabel) otherTerm);
            } else if (term instanceof KLabelInjection && otherTerm instanceof KLabelInjection) {
                unify((KLabelInjection) term, (KLabelInjection) otherTerm);
            } else if (term instanceof Hole && otherTerm instanceof Hole) {
                unify((Hole) term, (Hole) otherTerm);
            } else if (term instanceof Bottom && otherTerm instanceof Bottom) {
                unify((Bottom) term, (Bottom) otherTerm);
            } else if (term instanceof BuiltinList && otherTerm instanceof BuiltinList) {
                unify((BuiltinList) term, (BuiltinList) otherTerm);
            } else if (term instanceof BuiltinMap && otherTerm instanceof BuiltinMap) {
                unify((BuiltinMap) term, (BuiltinMap) otherTerm);
            } else if (term instanceof BuiltinSet && otherTerm instanceof BuiltinSet) {
                unify((BuiltinSet) term, (BuiltinSet) otherTerm);
            } else {
                fail(term, otherTerm);
                break;
                //if (!term.getClass().equals(otherTerm.getClass())) {
                //    throw KEMException.internalError(
                //            "mismatched types: " + term.getClass().getSimpleName()
                //            + " and " + otherTerm.getClass().getSimpleName());
                //} else {
                //    throw KEMException.internalError(
                //            "unexpected type: " + term.getClass().getSimpleName());
                //}
            }
        }
        return !failed;
    }

    abstract boolean stop(Term term, Term otherTerm);

    @Override
    public abstract void unify(Bottom bottom, Bottom term);
    @Override
    public abstract void unify(BuiltinList builtinList, BuiltinList term);
    @Override
    public abstract void unify(BuiltinMap builtinMap, BuiltinMap term);
    @Override
    public abstract void unify(BuiltinSet builtinSet, BuiltinSet term);
    @Override
    public abstract void unify(CellCollection cellCollection, CellCollection term);
    @Override
    public abstract void unify(Hole hole, Hole term);
    @Override
    public abstract void unify(KItem kItem, KItem term);
    @Override
    public abstract void unify(KLabelConstant kLabelConstant, KLabelConstant term);
    @Override
    public abstract void unify(KLabelInjection kLabelInjection, KLabelInjection term);
    @Override
    public abstract void unify(KList kList, KList term);
    @Override
    public abstract void unify(KSequence kSequence, KSequence term);
    @Override
    public abstract void unify(Token token, Token term);
    @Override
    public abstract void unify(InjectedKLabel injectedKLabel, InjectedKLabel term);
}