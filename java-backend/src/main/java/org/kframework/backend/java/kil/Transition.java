package org.kframework.backend.java.kil;

import java.util.Map;

/**
 * Transition in the Java Rewrite Engine.
 */
public class Transition {
    private Rule rule;
    private Map<Variable, Term> substitution;

    public Transition(Map<Variable, Term> substitution, Rule rule) {
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
