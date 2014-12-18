// Copyright (c) 2013-2014 K Team. All Rights Reserved.

package org.kframework.krun.api;

import org.kframework.kil.Term;
import org.kframework.kil.Variable;

import java.util.Map;

/**
 * Represents a transition in the debugger, contains extra information
 * about substitutions, apart from the rules.
 */
public class DebugTransition {
    private Transition transition;
    private Map<Variable, Term> substMap;


    public DebugTransition() {
    }

    public DebugTransition(Transition transition, Map<Variable, Term> substMap) {
        this.transition = transition;
        this.substMap = substMap;
    }

    public Transition getTransition() {
        return transition;
    }

    public void setTransition(Transition transition) {
        this.transition = transition;
    }

    public Map<Variable, Term> getSubstMap() {
        return substMap;
    }

    public void setSubstMap(Map<Variable, Term> substMap) {
        this.substMap = substMap;
    }
}
