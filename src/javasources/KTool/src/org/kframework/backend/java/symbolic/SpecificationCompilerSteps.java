package org.kframework.backend.java.symbolic;

import org.kframework.compile.transformers.*;
import org.kframework.compile.utils.CompileDataStructures;
import org.kframework.compile.utils.CompileToBuiltins;
import org.kframework.compile.utils.CompilerSteps;
import org.kframework.kil.Module;
import org.kframework.kil.loader.Context;


/**
 * @author: AndreiS
 */
public class SpecificationCompilerSteps extends CompilerSteps<Module> {

    public SpecificationCompilerSteps(Context context) {
        super(context);

        //add(new CheckVisitorStep<Definition>(new CheckConfigurationCells(context), context));
        add(new RemoveBrackets(context));
        add(new AddEmptyLists(context));
        add(new RemoveSyntacticCasts(context));
        //add(new CheckVisitorStep<Definition>(new CheckVariables(context), context));
        //add(new CheckVisitorStep<Definition>(new CheckRewrite(context), context));

        add(new AddKCell(context));
        add(new AddTopCellRules(context));
        add(new ResolveAnonymousVariables(context));
        add(new ResolveListOfK(context));
        add(new FlattenTerms(context));
        add(new FlattenSyntax(context));
        add(new ResolveContextAbstraction(context));
        add(new ResolveOpenCells(context));
        add(new ResolveRewrite(context));
        add(new CompileToBuiltins(context));
        add(new CompileDataStructures(context));
        //add(new DataStructureToLookupUpdate(context));
    }

}
