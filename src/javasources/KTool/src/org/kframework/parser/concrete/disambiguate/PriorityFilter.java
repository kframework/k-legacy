// Copyright (c) 2014 K Team. All Rights Reserved.
package org.kframework.parser.concrete.disambiguate;

import org.kframework.kil.KLabelConstant;
import org.kframework.kil.ASTNode;
import org.kframework.kil.Ambiguity;
import org.kframework.kil.KApp;
import org.kframework.kil.KList;
import org.kframework.kil.Production;
import org.kframework.kil.Sort;
import org.kframework.kil.Term;
import org.kframework.kil.TermCons;
import org.kframework.kil.loader.Context;
import org.kframework.kil.visitors.BasicTransformer;
import org.kframework.kil.visitors.exceptions.TransformerException;
import org.kframework.parser.concrete.disambiguate.PriorityFilter2.Side;

import java.util.List;

public class PriorityFilter extends BasicTransformer {
    public PriorityFilter(Context context) {
        super("Priority filter", context);
    }

    @Override
    public ASTNode transform(TermCons tc) throws TransformerException {
        if (tc.getProduction() == null)
            System.err.println(this.getClass() + ":" + " cons not found." + tc.getCons());
        String label = tc.getProduction().getKLabel();
        if (tc.getProduction().isListDecl()) {
            tc.getContents().set(0, (Term) tc.getContents().get(0).accept(new PriorityFilter2(label, Side.LEFT, context)));
            tc.getContents().set(1, (Term) tc.getContents().get(1).accept(new PriorityFilter2(label, Side.RIGHT, context)));
        } else if (!tc.getProduction().isConstant() && !tc.getProduction().isSubsort()) {
            for (int i = 0, j = 0; i < tc.getProduction().getItems().size(); i++) {
                if (tc.getProduction().getItems().get(i) instanceof Sort) {
                    // look for the outermost element
                    if ((i == 0 || i == tc.getProduction().getItems().size() - 1)
                            && (tc.getContents().get(j) instanceof TermCons || tc.getContents().get(j) instanceof Ambiguity)) {
                        tc.getContents().set(j, (Term) tc.getContents().get(j).accept(
                                new PriorityFilter2(label, i == 0 ? Side.LEFT : Side.RIGHT, context)));
                    }
                    j++;
                }
            }
        }

        return super.transform(tc);
    }

    @Override
    public ASTNode transform(KApp kapp) throws TransformerException {
        // get the production
        // look only for the outermost elements
        if (kapp.getLabel() instanceof KLabelConstant) {
            KLabelConstant kls = (KLabelConstant) kapp.getLabel();
            String label = kls.getLabel();
            List<Production> prods = context.productionsOf(label);
            if (prods.size() > 0) {
                // there are terms declared using only the klabel form
                // in that case don't do any priority disambiguation
                // TODO: check to see if the returned productions are overloaded (if more than one)
                Production prod = prods.get(0);
                if (kapp.getChild() instanceof  KList) {
                    KList klist = (KList) kapp.getChild();
                    assert klist.getContents().size() == prod.getArity() : "Production arity and KApp don't match";
                    for (int i = 0, j = 0; i < prod.getItems().size(); i++) {
                        if (prod.getItems().get(i) instanceof Sort) {
                            // look for the outermost element
                            if (i == 0 || i == prod.getItems().size() - 1) {
                                klist.getContents().set(j, (Term) klist.getContents().get(j).accept(
                                        new PriorityFilter2(label, i == 0 ? Side.LEFT : Side.RIGHT, context)));
                            }
                            j++;
                        }
                    }
                }
            }
        }

        return super.transform(kapp);
    }
}
