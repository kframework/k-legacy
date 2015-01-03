package org.kframework.krun.api;

import java.util.Optional;

/**
 * Rewrite Relation object contains the results of a KRun.
 * If the graph Trace is enabled, then the relation also
 * contains the graph of the run.
 */
public class RewriteRelation {

    private KRunState finalState;

    private Optional<KRunGraph> executionGraph;

    public RewriteRelation(KRunState finalState) {
        this.finalState = finalState;
    }

    public RewriteRelation(KRunState finalState, KRunGraph executionTraceGraph) {
        this.finalState = finalState;
        this.executionGraph = Optional.of(executionTraceGraph);
    }
    public KRunState getFinalState() {
        return finalState;
    }

    public void setFinalState(KRunState finalState) {
        this.finalState = finalState;
    }

    public Optional<KRunGraph> getExecutionGraph() {
        return executionGraph;
    }

    public void setExecutionGraph(KRunGraph executionGraph) {
        this.executionGraph = Optional.ofNullable(executionGraph);
    }
}
