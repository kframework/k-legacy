// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.backend.java.symbolic;

import org.kframework.backend.java.kil.Term;
import org.kframework.backend.java.kil.State;


/**
 * Evaluates functions/predicates and collects symbolic constraint generated
 * in the evaluation process.
 */
public class Evaluator extends PrePostTransformer {

    private Evaluator(SymbolicConstraint constraint, State state) {
        super(state);
        this.getPostTransformer().addTransformer(new LocalEvaluator(constraint, state));
    }
    
    public static Term evaluate(Term term, State state) {
        return Evaluator.evaluate(term, null, state);
    }

    public static Term evaluate(Term term, SymbolicConstraint constraint,
            State state) {
        Evaluator evaluator = new Evaluator(constraint, state);
        return (Term) term.accept(evaluator);
    }
}
