// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.parser.concrete.disambiguate;

import java.util.*;

import org.kframework.compile.utils.MetaK;
import org.kframework.kil.Ambiguity;
import org.kframework.kil.Term;
import org.kframework.kil.TermCons;
import org.kframework.kil.Variable;
import org.kframework.kil.loader.Context;
import org.kframework.kil.visitors.BasicVisitor;

public class CollectExpectedVariablesVisitor extends BasicVisitor {
    public CollectExpectedVariablesVisitor(Context context) {
        super(context);
    }

    /**
     * Each element in the list is a Mapping from variable name and a list of constraints for that variable.
     * On each Ambiguity node, a cartesian product is created between the current List and each ambiguity variant.
     */
    public Set<VarHashMap> vars = new HashSet<>();

    @Override
    public Void visit(Ambiguity node, Void _) {
        Set<VarHashMap> newVars = new HashSet<>();
        LookAlikeMap visitors = new LookAlikeMap(context);
        // first visit all the children and collect the visitors
        // the visitors will be shared for similar productions
        for (Term t : node.getContents())
            visitors.get(t).visitNode(t);
        for (CollectExpectedVariablesVisitor viz : visitors.values()) {
            // create the split
            for (VarHashMap elem : vars) { // for every local type restrictions
                for (VarHashMap elem2 : viz.vars) { // create a combination with every ambiguity detected
                    newVars.add(elem.deepClone().addAll(elem2));
                }
            }
            if (vars.size() == 0)
                newVars.addAll(viz.vars);

        }
        if (!newVars.isEmpty())
            vars = newVars;
        return visit((Term) node, _);
    }

    @Override
    public Void visit(Variable var, Void _) {
        if (!var.isUserTyped() && !var.getName().equals(MetaK.Constants.anyVarSymbol)) {
            if (vars.isEmpty())
                vars.add(new VarHashMap());
            for (VarHashMap vars2 : vars)
                vars2.add(var.getName(), var.getLocation(), var.getExpectedSort());
        }
        return null;
    }

    /**
     * A mapping from a term that has a specific pattern, to the variable collect visitor.
     * The pattern can be:
     *   - a Variable
     *   - a TermCons which looks alike with another TermCons
     *       the two terms can be alike if all children have the same location information
     *       and if the productions are subsorted or equal one to another.
     *       This is useful because overloaded operators need special handling.
     */
    private class LookAlikeMap extends HashMap<Term, CollectExpectedVariablesVisitor> {
        private final Context context;
        public LookAlikeMap(Context context) {
            this.context = context;
        }

        public CollectExpectedVariablesVisitor get(Term t) {
            for (Term trm : this.keySet()) {
                if (trm instanceof Variable && t instanceof Variable)
                    return super.get(trm);
                if (trm instanceof TermCons && t instanceof TermCons)
                    if (context.isSubsortedEq((TermCons) trm, (TermCons) t) ||
                        context.isSubsortedEq((TermCons) t, (TermCons) trm)) {
                        //System.out.println("Found dito: " + ((TermCons) trm).getProduction());
                        return super.get(trm);
                    }
            }
            // couldn't find anything that looks alike, create a new instance of the visitor
            CollectExpectedVariablesVisitor viz = new CollectExpectedVariablesVisitor(context);
            this.put(t, viz);
            return viz;
        }
    }
}
