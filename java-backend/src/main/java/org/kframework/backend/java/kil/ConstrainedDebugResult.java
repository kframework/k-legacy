// Copyright (c) 2014 K Team. All Rights Reserved.

package org.kframework.backend.java.kil;

import java.util.Map;

/**
 *  Debugger Result object to be used in the symbolic rewriter.
 *  Internal Representation to be used in the java rewrite engine.
 */
public class ConstrainedDebugResult {

    private ConstrainedTerm steppedState;
    private Rule rule;
    private Map<Variable, Term> substitutionMap;

    public ConstrainedDebugResult(ConstrainedTerm steppedState, Rule rule, Map<Variable, Term> substitutionMap) {
        this.steppedState = steppedState;
        this.rule = rule;
        this.substitutionMap = substitutionMap;
    }

    public ConstrainedTerm getSteppedState() {
        return steppedState;
    }

    public void setSteppedState(ConstrainedTerm steppedState) {
        this.steppedState = steppedState;
    }

    public Rule getRule() {
        return rule;
    }

    public void setRule(Rule rule) {
        this.rule = rule;
    }

    public Map<Variable, Term> getSubstitutionMap() {
        return substitutionMap;
    }

    public void setSubstitutionMap(Map<Variable, Term> substitutionMap) {
        this.substitutionMap = substitutionMap;
    }
}
