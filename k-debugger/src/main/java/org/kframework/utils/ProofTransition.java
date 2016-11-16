package org.kframework.utils;

import org.jgrapht.graph.DefaultEdge;

public class ProofTransition extends DefaultEdge {

    private int steps;

    private String label;

    public ProofTransition(int steps, String label) {
        this.steps = steps;
        this.label = label;
    }

    public int getSteps() {
        return steps;
    }

    public String getLabel() {
        return label;
    }
}
