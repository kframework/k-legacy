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

    @Override
    public Void visit(Definition definition, Void _) {
        super.visit(definition, _);
        getCurrentDefinition().numModules = 0;
        getCurrentDefinition().numSentences = 0;
        getCurrentDefinition().numProductions = 0;
        getCurrentDefinition().numCells = 0;
        return null;
    }

    @Override
    public Void visit(Module module, Void _) {
        if (!module.isPredefined()) {
            getCurrentDefinition().numModules++;
            super.visit(module, _);
        }
        return null;
    }

    @Override
    public Void visit(Sentence rule, Void _) {
        getCurrentDefinition().numSentences++;
        return super.visit(rule, _);
    }

    @Override
    public Void visit(Production production, Void _) {
        getCurrentDefinition().numProductions++;
        return super.visit(production, _);
    }

    boolean inConfig = false;

    @Override
    public Void visit(Configuration config, Void _) {
        inConfig = true;
        super.visit(config, _);
        inConfig = false;
        getCurrentDefinition().numSentences++;
        return null;
    }

    @Override
    public Void visit(Cell cell, Void _) {
        if (inConfig) {
            getCurrentDefinition().numCells++;
            super.visit(cell, _);
        }
        return null;
    }
}
