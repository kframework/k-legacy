// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.kil.loader;

import org.kframework.kil.Cell;
import org.kframework.kil.Configuration;
import org.kframework.kil.Definition;
import org.kframework.kil.Module;
import org.kframework.kil.Production;
import org.kframework.kil.Sentence;

import org.kframework.kil.visitors.BasicVisitor;

public class CountNodesVisitor extends BasicVisitor {

    public int numModules = 0, numSentences = 0, numProductions = 0, numCells = 0;

    @Override
    public Void visit(Definition definition, Void _) {
        super.visit(definition, _);
        return null;
    }

    @Override
    public Void visit(Module module, Void _) {
        if (!module.isPredefined()) {
            numModules++;
            super.visit(module, _);
        }
        return null;
    }

    @Override
    public Void visit(Sentence rule, Void _) {
        numSentences++;
        return super.visit(rule, _);
    }

    @Override
    public Void visit(Production production, Void _) {
        numProductions++;
        return super.visit(production, _);
    }

    boolean inConfig = false;

    @Override
    public Void visit(Configuration config, Void _) {
        inConfig = true;
        super.visit(config, _);
        inConfig = false;
        numSentences++;
        return null;
    }

    @Override
    public Void visit(Cell cell, Void _) {
        if (inConfig) {
            numCells++;
            super.visit(cell, _);
        }
        return null;
    }
}
