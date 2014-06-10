// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.backend.java.symbolic;

import org.kframework.backend.java.kil.Term;
import org.kframework.backend.java.kil.State;


/**
 * Evaluates functions/predicates and collects symbolic constraint generated
 * in the evaluation process.
 */
public class Evaluator extends PrePostTransformer {

    private Evaluator(SymbolicConstraint constraint, State context) {
        super(context);
        this.getPostTransformer().addTransformer(new LocalEvaluator(constraint, context));
    }
    
    public static Term evaluate(Term term, State context) {
        return Evaluator.evaluate(term, null, context);
    }

    public static Term evaluate(Term term, SymbolicConstraint constraint,
            State context) {
        Evaluator evaluator = new Evaluator(constraint, context);
        return (Term) term.accept(evaluator);
    }
}
