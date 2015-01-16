// Copyright (c) 2014-2015 K Team. All Rights Reserved.
package org.kframework.backend.java.kil;

import java.util.Map;

/**
 * A Transition in the Java Backend for the symoblic rewriter.
 */
public class JavaTransition {
    private Rule rule;
    private Map<Variable, Term> substitution;

    public JavaTransition(Map<Variable, Term> substitution, Rule rule) {
        this.substitution = substitution;
        this.rule = rule;
    }

    public Rule getRule() {
        return rule;
    }

    public void setRule(Rule rule) {
        this.rule = rule;
    }

    public Map<Variable, Term> getSubstitution() {
        return substitution;
    }

    public void setSubstitution(Map<Variable, Term> substitution) {
        this.substitution = substitution;
    }
}
