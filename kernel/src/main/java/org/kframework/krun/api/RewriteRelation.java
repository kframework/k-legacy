// Copyright (c) 2014-2015 K Team. All Rights Reserved.
package org.kframework.krun.api;


import java.util.Optional;

/**
 * The result of a step operation in the debugger.
 * Contains the step/search graph if the compute graph option is specified.
 */
public class RewriteRelation {
    private KRunState finalState;

    Optional<KRunGraph> executionGraph;

    public RewriteRelation(KRunState finalState, KRunGraph executionGraph) {
        this.finalState = finalState;
        this.executionGraph = Optional.ofNullable(executionGraph);
    }

    public KRunState getFinalState() {
        return finalState;
    }

    public void setFinalState(KRunState finalState) {
        this.finalState = finalState;
    }

    public KRunGraph getExecutionGraph() {
        return executionGraph.get();
    }

    public void setExecutionGraph(KRunGraph executionGraph) {
        this.executionGraph = Optional.ofNullable(executionGraph);
    }
}
