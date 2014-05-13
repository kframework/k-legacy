// Copyright (c) 2012-2014 K Team. All Rights Reserved.
package org.kframework.kil.loader;

import org.kframework.kil.Cell;
import org.kframework.kil.Configuration;
import org.kframework.kil.Sentence;
import org.kframework.kil.visitors.BasicVisitor;

public class CollectConfigCellsVisitor extends BasicVisitor {
    public CollectConfigCellsVisitor(Context context) {
        super(context);
    }

    @Override
    public void visit(Configuration config) {
        super.visit((Sentence) config);
    }

    @Override
    public void visit(Sentence s) {
    }

    @Override
    public void visit(Cell cell) {
        context.addCellDecl(cell);
        super.visit(cell);
    }
}
