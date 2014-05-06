// Copyright (c) 2014 K Team. All Rights Reserved.
package org.kframework.parser.concrete.disambiguate;

import java.util.ArrayList;

import org.kframework.kil.ASTNode;
import org.kframework.kil.Ambiguity;
import org.kframework.kil.KApp;
import org.kframework.kil.KLabelConstant;
import org.kframework.kil.Term;
import org.kframework.kil.TermCons;
import org.kframework.kil.loader.Context;
import org.kframework.kil.visitors.BasicHookWorker;
import org.kframework.kil.visitors.exceptions.PriorityException;
import org.kframework.kil.visitors.exceptions.TransformerException;
import org.kframework.utils.errorsystem.KException;
import org.kframework.utils.errorsystem.KException.ExceptionType;
import org.kframework.utils.errorsystem.KException.KExceptionGroup;

public class PriorityFilter2 extends BasicHookWorker {

    /**
     * Specifies whether the current node is the left most or the right most child of the parent.
     * This is useful because associativity can be checked at the same time with priorities.
     */
    public static enum Side {LEFT, RIGHT}
    private String parentLabel;
    private Side side;

    public PriorityFilter2(String parentLabel, Side side, Context context) {
        super("Type system", context);
        this.parentLabel = parentLabel;
        this.side = side;
    }
/*
    public PriorityFilter2(PriorityFilter2 pf, Context context) {
        super("Type system", context);
        this.parentLabel = pf.parentLabel;
        this.side = pf.side;
    }
*/
    public ASTNode transform(TermCons tc) throws TransformerException {
        String localLabel = tc.getProduction().getKLabel();
        if (context.isPriorityWrong(parentLabel, localLabel)) {
            String msg = "Priority filter exception. Cannot use " + localLabel + " as a child of " + parentLabel;
            KException kex = new KException(ExceptionType.ERROR, KExceptionGroup.CRITICAL, msg, tc.getFilename(), tc.getLocation());
            throw new PriorityException(kex);
        }

        return tc;
    }

    public ASTNode transform(KApp kapp) throws TransformerException {
        if (kapp.getLabel() instanceof KLabelConstant) {
            KLabelConstant kls = (KLabelConstant) kapp.getLabel();
            String localLabel = kls.getLabel();
            // TODO (Radu): check for associativity too
            if (context.isPriorityWrong(parentLabel, localLabel)) {
                String msg = "Priority filter exception. Cannot use " + localLabel + " as a child of " + parentLabel;
                KException kex = new KException(ExceptionType.ERROR, KExceptionGroup.CRITICAL, msg, kapp.getFilename(), kapp.getLocation());
                throw new PriorityException(kex);
            }
        }

        return kapp;
    }

    @Override
    public ASTNode transform(Ambiguity node) throws TransformerException {
        TransformerException exception = null;
        ArrayList<Term> terms = new ArrayList<Term>();
        for (Term t : node.getContents()) {
            ASTNode result = null;
            try {
                result = t.accept(this);
                terms.add((Term) result);
            } catch (TransformerException e) {
                exception = e;
            }
        }
        if (terms.isEmpty())
            throw exception;
        if (terms.size() == 1) {
            return terms.get(0);
        }
        node.setContents(terms);
        return node;
    }

    @Override
    public ASTNode transform(Term node) throws TransformerException {
        return node;
    }
}
