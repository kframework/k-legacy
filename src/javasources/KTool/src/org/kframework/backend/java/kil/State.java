// Copyright (c) 2014-2014 K Team. All Rights Reserved.
package org.kframework.backend.java.kil;

import java.math.BigInteger;

/**
 * State represents a state in the rewrite system.
 * 
 * The aim is for State to be immutable. It is not transitively immutable
 * because the global state links to the mutable file system.
 */

public class State<T extends TermLike> {
    public final T topTerm;
    public final GlobalContext global;
    public final BigInteger counter;
    public final boolean isStuck;    

    public State(T topTerm, GlobalContext global,
            BigInteger counter, boolean isStuck) {
        this.topTerm = topTerm;
        this.global = global;
        this.counter = counter;
        this.isStuck = isStuck;
    }
    
    public State(T topTerm, GlobalContext global) {
        this(topTerm, global, BigInteger.ZERO, false);
    }
    
    public State<T> incrementCounter() {
        return new State<T>(topTerm, global, counter.add(BigInteger.ONE), isStuck);
    }
    
    public <U extends TermLike> State<U> copy(U topTerm) {
        return new State<U>(topTerm, global, counter, isStuck);
    }
    
    public State<T> stuck() {
        return new State<T>(topTerm, global, counter, true);
    }
}