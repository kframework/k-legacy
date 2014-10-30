// Copyright (c) 2012-2014 K Team. All Rights Reserved.
package org.kframework.parser.concrete.disambiguate;

import org.kframework.kil.Definition;
import org.kframework.kil.Module;
import org.kframework.kil.loader.Context;
import org.kframework.kil.visitors.BasicVisitor;

public abstract class GetFitnessUnitBasicVisitor extends BasicVisitor {
    public GetFitnessUnitBasicVisitor(Context context) {
        super(context);
    }

    public GetFitnessUnitBasicVisitor(Context context, Definition currentDefinition,
                                      Module currentModule) {
        super(context, currentDefinition, currentModule);
    }

    protected int score = 0;

    public int getScore() {
        return score;
    }

    public abstract GetFitnessUnitBasicVisitor getInstance();
}
