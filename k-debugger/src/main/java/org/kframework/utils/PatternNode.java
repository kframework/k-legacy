package org.kframework.utils;


import org.kframework.kore.K;

public class PatternNode {
    private K pattern;
    private int id;

    public PatternNode(K pattern, int id) {
        this.pattern = pattern;
        this.id = id;
    }

    public K getPattern() {
        return pattern;
    }

    public int getId() {
        return id;
    }
}
