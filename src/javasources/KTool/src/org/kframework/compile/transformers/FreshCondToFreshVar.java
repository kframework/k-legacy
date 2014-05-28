// Copyright (c) 2012-2014 K Team. All Rights Reserved.
package org.kframework.compile.transformers;

import org.kframework.compile.utils.MetaK;
import org.kframework.compile.utils.Substitution;
import org.kframework.kil.*;
import org.kframework.kil.loader.Context;
import org.kframework.kil.visitors.CopyOnWriteTransformer;
import org.kframework.utils.errorsystem.KException;
import org.kframework.utils.general.GlobalSettings;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class FreshCondToFreshVar extends CopyOnWriteTransformer {

    private Set<Variable> vars = new HashSet<Variable>();

    public FreshCondToFreshVar(Context context) {
        super("Transform fresh conditions into fresh variables.", context);
    }

    @Override
    public ASTNode visit(Sentence node, Void _)  {
        //TODO:  maybe now fresh belongs in the ensures?  update this accordingly if so.
        if (null == node.getRequires())
            return node;

        vars.clear();
        ASTNode condNode = this.visitNode(node.getRequires());
        if (vars.isEmpty())
            return node;

        node = node.shallowCopy();
        node.setRequires((Term) condNode);

        ASTNode bodyNode = freshSubstitution(vars).visitNode(node.getBody());
        assert(bodyNode instanceof Term);
        node.setBody((Term)bodyNode);
        
        return node;
    }
    
    /**
     * Change this class to receive a KApp instead a TermCons since We will move FlattenTerms before
     * This step, then TermCons will not exist. 
     */
    @Override
    public ASTNode visit(KApp node, Void _)  {
        if ((node.getLabel() instanceof KLabelConstant) 
                && MetaK.Constants.freshLabel.equals(((KLabelConstant)(node.getLabel())).getLabel())) {
            if (!(node.getChild() instanceof KList)) {
                GlobalSettings.kem.register(new KException(KException.ExceptionType.WARNING,
                        KException.KExceptionGroup.COMPILER,
                        "KApp has a non-KList item in a KList position:" + node,
                        getName(), node.getFilename(), node.getLocation()));
            }
            if (((KList)(node.getChild())).getContents().size() != 1) {
                GlobalSettings.kem.register(new KException(KException.ExceptionType.WARNING,
                        KException.KExceptionGroup.COMPILER,
                        "Fresh has more than one argument:" + node,
                        getName(), node.getFilename(), node.getLocation()));
            }
            if (!(((KList)(node.getChild())).getContents().get(0) instanceof Variable)) {
                GlobalSettings.kem.register(new KException(KException.ExceptionType.WARNING,
                        KException.KExceptionGroup.COMPILER,
                        "Fresh must take a variable as argument:" + node,
                        getName(), node.getFilename(), node.getLocation()));
            }
            Variable var = (Variable) ((KList)(node.getChild())).getContents().get(0);
            this.vars.add(var);
            return BoolBuiltin.TRUE;
        }

        return super.visit(node, _);
    }

    private Substitution freshSubstitution(Set<Variable> vars) {
        Map<Term, Term> symMap = new HashMap<Term, Term>();
        for (Variable var : vars) {
            Variable freshVar = var.shallowCopy();
            // TODO: this class should become dead code
            //freshVar.setFreshVariable(true);
            symMap.put(var, freshVar);
        }

        return new Substitution(symMap, context);
    }
}

