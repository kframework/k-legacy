// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.utils;

import org.jgrapht.DirectedGraph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.kframework.definition.Rule;

public class Goal {
    private Rule goalPattern;
    private boolean proved;
    private DirectedGraph<PatternNode, ProofTransition> proofTree;

    public Goal(Rule goalPattern, boolean proved) {
        this.goalPattern = goalPattern;
        this.proved = proved;
        this.proofTree = new DefaultDirectedGraph<PatternNode, ProofTransition>(ProofTransition.class);
//        proofTree.addVertex()
    }
}