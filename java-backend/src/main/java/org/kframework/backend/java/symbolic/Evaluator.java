// Copyright (c) 2013-2015 K Team. All Rights Reserved.
package org.kframework.backend.java.symbolic;

import org.kframework.backend.java.kil.KItem;
import org.kframework.backend.java.kil.KItemProjection;
import org.kframework.backend.java.kil.KLabel;
import org.kframework.backend.java.kil.Term;
import org.kframework.backend.java.kil.TermContext;
import org.kframework.kil.ASTNode;


/**
 * Evaluates pending functions inside a term.
 */
public class Evaluator extends CopyOnWriteTransformer {

    private KLabel exception;

    public Evaluator(TermContext context, KLabel exception) {
        super(context);
        this.exception = exception;
    }

    public static Term evaluate(Term term, TermContext context, KLabel exception) {
        Evaluator evaluator = new Evaluator(context, exception);
        return (Term) term.accept(evaluator);
    }

    @Override
    public ASTNode transform(KItem kItem) {
        return ((KItem) super.transform(kItem)).resolveFunctionAndAnywhere(false, context, exception);
    }

    @Override
    public ASTNode transform(KItemProjection kItemProjection) {
        return ((KItemProjection) super.transform(kItemProjection)).evaluateProjection();
    }

}
