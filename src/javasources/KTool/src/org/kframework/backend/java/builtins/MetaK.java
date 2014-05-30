// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.backend.java.builtins;

import org.kframework.backend.java.kil.BuiltinMap;
import org.kframework.backend.java.kil.BuiltinSet;
import org.kframework.backend.java.kil.KItem;
import org.kframework.backend.java.kil.KLabelInjection;
import org.kframework.backend.java.kil.KList;
import org.kframework.backend.java.kil.MetaVariable;
import org.kframework.backend.java.kil.Term;
import org.kframework.backend.java.kil.State;
import org.kframework.backend.java.kil.Variable;
import org.kframework.backend.java.symbolic.PatternMatcher;
import org.kframework.backend.java.symbolic.SymbolicConstraint;
import org.kframework.backend.java.symbolic.SymbolicUnifier;
import org.kframework.backend.java.symbolic.UnificationFailure;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;


/**
 * Table of {@code public static} methods for builtin meta K operations.
 * 
 * @author AndreiS
 */
public class MetaK {

    /**
     * Checks if two given {@link Term}s can be unified.
     * 
     * @param term1
     *            the first term
     * @param term2
     *            the second term
     * @param state
     *            the term state
     * @return {@link BoolToken#TRUE} if the two terms can be unified;
     *         otherwise, {@link BoolToken#FALSE}
     */
    public static BoolToken unifiable(Term term1, Term term2, State state) {
//        Term freshTerm1 = term1.substitute(Variable.getFreshSubstitution(term1.variableSet()), state);
//        Term freshTerm2 = term2.substitute(Variable.getFreshSubstitution(term2.variableSet()), state);
        SymbolicConstraint constraint = new SymbolicConstraint(state);
        SymbolicUnifier unifier = new SymbolicUnifier(constraint, state);
        try {
            unifier.unify(term1, term2);
        } catch (UnificationFailure e) {
            return BoolToken.FALSE;
        }
        return BoolToken.TRUE;
    }

    /**
     * Checks if the subject term matches the pattern.
     * 
     * @param subject
     *            the subject term
     * @param pattern
     *            the pattern term
     * @param state
     *            the term state
     * @return {@link BoolToken#TRUE} if the two terms can be matched;
     *         otherwise, {@link BoolToken#FALSE}
     */
    public static BoolToken matchable(Term subject, Term pattern, State state) {
        return PatternMatcher.matchable(subject, pattern, state) ? BoolToken.TRUE
                : BoolToken.FALSE;
    }
    
    /**
     * Renames {@link Variable}s of a given {@link Term} if they appear also in
     * a given {@link BuiltinSet} of {@link MetaVariable}s.
     * 
     * 
     * @param term
     *            the given term
     * @param builtinSet
     *            the given set of meta variables
     * @param state
     *            the term state
     * @return the resulting term if the renaming succeeds; or the original term
     *         if the given {@code BuiltinSet} has a frame or contains not only
     *         {@code MetaVariable}s
     */
    public static Term rename(Term term, BuiltinSet builtinSet, State state) {
        if (builtinSet.hasFrame() /* || !builtinSet.operations().isEmpty() */) {
            return term;
        }

        Set<Variable> variables = new HashSet<Variable>();
        for (Term element : builtinSet.elements()) {
            if (!(element instanceof MetaVariable)) {
                return term;
            }

            variables.add(new Variable((MetaVariable) element));
        }

        return term.substitute(Variable.getFreshSubstitution(variables), state);
    }

    /**
     * Renames all {@link Variable}s inside a given {@link Term} to unique fresh names.
     * 
     * @param term
     *            the given term
     * @param state
     *            the term state
     * @return the resulting term after renaming
     */
    public static Term renameVariables(Term term, State state) {
        Set<Variable> variables = term.variableSet();
        return term.substitute(Variable.getFreshSubstitution(variables), state);
    }

    /**
     * Returns all {@link Variable}s inside a given {@link Term} as a
     * {@link BuiltinSet} of {@link MetaVariable}s.
     * 
     * @param term
     *            the given term
     * @param state
     *            the term state
     * @return a {@code BuiltinSet} of {@code MetaVariable}s
     */
    public static BuiltinSet variables(Term term, State state) {
        Set<Term> metaVariables = new HashSet<Term>();
        for (Variable variable : term.variableSet()) {
            metaVariables.add(new MetaVariable(variable));
        }
        return new BuiltinSet(metaVariables);
    }
    
    /**
     * Returns all {@link Variable}s inside a given {@link Term} as a
     * {@link BuiltinSet}.
     * 
     * @param term
     *            the given term
     * @param state
     *            the term state
     * @return a {@code BuiltinSet} of {@code Variable}s
     */
    public static BuiltinSet trueVariables(Term term, State state) {
        Set<Variable> trueVariables = term.variableSet();
        return new BuiltinSet(trueVariables);
    }

    public static BuiltinMap variablesMap(Term term, State state) {
        Set<Variable> variables = term.variableSet();
        Map<MetaVariable, Variable> result = new HashMap<>(variables.size());
        for (Variable variable : variables) {
            assert variable instanceof Variable : "this function only applies on variables";
            result.put(new MetaVariable(variable), variable);
        }
        return BuiltinMap.of(result, null);
    }

    /**
     * Returns the K label of a specified {@link KItem}.
     * 
     * @param kItem
     *            the specified {@code KItem}
     * @param state
     *            the term state
     * @return the K label
     */
    public static KItem getKLabel(KItem kItem, State state) {
        // TODO(AndreiS): handle KLabel variables
        return new KItem(new KLabelInjection(kItem.kLabel()), new KList(), state);
    }
}
