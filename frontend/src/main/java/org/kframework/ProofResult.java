package org.kframework;

import org.kframework.frontend.K;

import java.util.List;

public class ProofResult {
    private List<K> results;

    private Status status;

    public enum Status {
        PROVED, NOT_PROVED, EMPTY_SPEC
    }

    public ProofResult(List<K> results, Status status) {
        this.results = results;
        this.status = status;
    }

    public List<K> getResults() {
        return results;
    }

    public Status getStatus() {
        return status;
    }
}
