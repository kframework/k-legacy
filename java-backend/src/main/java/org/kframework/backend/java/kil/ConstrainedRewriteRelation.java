package org.kframework.backend.java.kil;

import java.util.Optional;

/**
 * Backend Representation of the Rewrite Relation represented in the
 * Frontend by the rewrite relation class.
 * Used by the Symbolic Rewriter, and then converted to the generic notation by
 * an implementation of the executor interface.
 */
public class ConstrainedRewriteRelation {
    ConstrainedTerm finalTerm;

    Optional<ConstrainedExecutionGraph> constrainedExecutionGraph;

    public ConstrainedRewriteRelation() {
    }

    public ConstrainedRewriteRelation(ConstrainedTerm finalTerm, ConstrainedExecutionGraph constrainedExecutionGraph) {
        this.finalTerm = finalTerm;
        this.constrainedExecutionGraph = Optional.ofNullable(constrainedExecutionGraph);
    }


    public ConstrainedTerm getFinalTerm() {
        return finalTerm;
    }

    public void setFinalTerm(ConstrainedTerm finalTerm) {
        this.finalTerm = finalTerm;
    }

    public Optional<ConstrainedExecutionGraph> getConstrainedExecutionGraph() {
        return constrainedExecutionGraph;
    }

    public void setConstrainedExecutionGraph(ConstrainedExecutionGraph constrainedExecutionGraph) {
        this.constrainedExecutionGraph = Optional.ofNullable(constrainedExecutionGraph);
    }
}