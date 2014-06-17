// Copyright (c) 2014 K Team. All Rights Reserved.
package org.kframework.backend.java.symbolic;

import org.kframework.backend.java.kil.KItem;
import org.kframework.backend.java.kil.Term;
import org.kframework.backend.java.kil.TermContext;
import org.kframework.kil.ASTNode;


/**
 * Expands map patterns according to their definitions.
 */
public class PatternExpander extends PrePostTransformer {

    private PatternExpander(SymbolicConstraint constraint, TermContext context) {
        super(context);
        this.getPostTransformer().addTransformer(new LocalPatternExpander(constraint, context));
    }

    public static Term expand(Term term, SymbolicConstraint constraint, TermContext context) {
        return (Term) term.accept(new PatternExpander(constraint, context));
    }

    private static class LocalPatternExpander extends LocalTransformer {

        /**
        * The symbolic constraint of the {@code ConstrainedTerm} which contains the
        * terms to be evaluated by this evaluator.
        */
        private final SymbolicConstraint constraint;

        public LocalPatternExpander(SymbolicConstraint constraint, TermContext context) {
            super(context);
            this.constraint = constraint;
        }

        public ASTNode transform(KItem kItem) {
            return kItem.expandPattern(constraint, context);
        }

    }
}
