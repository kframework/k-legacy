// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.utils;

import org.jgrapht.DirectedGraph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.kframework.definition.Rule;
import org.kframework.kore.K;
import org.kframework.kore.KApply;
import org.kframework.kore.Unapply;

public class Goal {
    private Rule goalClaim;
    private boolean proved;
    private DirectedGraph<PatternNode, ProofTransition> proofTree;
    int nodeIds;

    public Goal(Rule goalClaim, boolean proved) {
        this.goalClaim = goalClaim;
        this.proved = proved;
        this.proofTree = new DefaultDirectedGraph<PatternNode, ProofTransition>(ProofTransition.class);
        this.nodeIds = 0;
        proofTree.addVertex(new PatternNode(getRuleLHS(goalClaim), nodeIds++));
    }

    public static K getRuleLHS(Rule rule) {
        return ((KApply) rule.body()).items().get(0);
    }

    public static K getRuleRHS(Rule rule) {
        return ((KApply) rule.body()).items().get(1);
    }

    public Rule getGoalClaim() {
        return goalClaim;
    }

    public boolean isProved() {
        return proved;
    }

    public DirectedGraph<PatternNode, ProofTransition> getProofTree() {
        return proofTree;
    }

    public int getNodeIds() {
        return nodeIds;
    }

    public void setNodeIds(int nodeIds) {
        this.nodeIds = nodeIds;
    }
}