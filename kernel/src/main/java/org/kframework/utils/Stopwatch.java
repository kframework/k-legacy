// Copyright (c) 2014-2016 K Team. All Rights Reserved.
package org.kframework.utils;

import org.kframework.main.GlobalOptions;

import java.util.Formatter;

/**
 * To use, access {@link #instance()} after calling {@link #init(GlobalOptions) init()}.
 */
public class Stopwatch {
    private long start;
    private long lastIntermediate;
    Formatter f = new Formatter(System.out);
    private final GlobalOptions options;

    public Stopwatch(GlobalOptions options) {
        this.options = options;
        start = System.currentTimeMillis();
        lastIntermediate = start;
    }

    public void start() {
        printIntermediate("Init");
    }

    public void printIntermediate(String message) {
        long current = System.currentTimeMillis();
        if (options.verbose)
            f.format("%-60s = %5d%n", message, current - lastIntermediate);
        lastIntermediate = current;
    }

    public void printTotal(String message) {
        printIntermediate("Cleanup");
        if (options.verbose)
            f.format("%-60s = %5d%n", message, lastIntermediate - start);
    }

    public long getIntermediateMilliseconds() {
        long endd = System.currentTimeMillis();
        long rez = lastIntermediate - endd;
        lastIntermediate = endd;
        return rez;
    }
}
