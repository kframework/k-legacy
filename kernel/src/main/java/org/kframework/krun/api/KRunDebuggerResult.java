// Copyright (c) 2014 K Team. All Rights Reserved.

package org.kframework.krun.api;

import java.util.Map;
import org.kframework.kil.Term;
import org.kframework.kil.Variable;

/**
 * Contains information about a debugger step operation.
 */
public class KRunDebuggerResult {

    private KRunState steppedState;
    private Transition rule;
    private Map<Variable, Term> substMap;


    public KRunDebuggerResult() {
        steppedState = null;
        rule = null;
        substMap = null;
    }

    public KRunDebuggerResult(KRunState steppedState, Transition rule, Map<Variable, Term> substMap) {
        this.steppedState = steppedState;
        this.rule = rule;
        this.substMap = substMap;
    }

    public KRunState getSteppedState() {
        return steppedState;
    }

    public void setSteppedState(KRunState steppedState) {
        this.steppedState = steppedState;
    }

    public Transition getRule() {
        return rule;
    }

    public void setRule(Transition rule) {
        this.rule = rule;
    }

    public Map<Variable, Term> getSubstMap() {
        return substMap;
    }

    public void setSubstMap(Map<Variable, Term> substMap) {
        this.substMap = substMap;
    }
}
