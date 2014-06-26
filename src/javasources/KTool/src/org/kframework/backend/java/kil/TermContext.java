// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.backend.java.kil;

import java.math.BigInteger;

import org.kframework.backend.java.symbolic.Transformer;
import org.kframework.backend.java.symbolic.Visitor;
import org.kframework.kil.ASTNode;
import org.kframework.krun.api.io.FileSystem;

/**
 * An object containing context specific to a term.
 * 
 * For now, it only contains a reference to a State object.
 * The reference to global is just a shortcut to the global in State.
 * 
 * As an optimization (and because it would be hard to refactor now), 
 * TermContext is mutable. The mutability is required by the counter,
 * as all invocations of #fresh cannot be applied atomically. As the 
 * counter is part of State, the state reference is updated each time
 * the counter is incremented. 
 * 
 */
public class TermContext extends JavaSymbolicObject {
    private State<TermLike> state;
    public final GlobalContext global;

    private TermContext(GlobalContext global, BigInteger counter) {
        this(new State(null, global, counter, false));
    }
    
    private TermContext(State state) {
        this.state = state;
        this.global = state.global;
    }
    
    public static <T extends Term> TermContext of(State<T> state) {
        return new TermContext(state);
    }

    public static TermContext of(GlobalContext global, BigInteger counter) {
        return new TermContext(global, counter);
    }
    
    public static TermContext of(GlobalContext global) {
        return new TermContext(global, BigInteger.ZERO);
    }
    
    public BigInteger getCounter() {
        return state.counter;
    }

    /**
     * Increments the fresh variable counter. 
     * 
     * See TermContext's class javadoc for details on the underlying implementation.
     */
    public BigInteger incrementCounter() {
        state = state.incrementCounter();
        return state.counter;
    }

    public Definition definition() {
        return state.global.def;
    }

    public FileSystem fileSystem() {
        return state.global.fs;
    }

    public TermLike getConstrainedTermData() {
        return state.topTerm;
    }

    public void setConstrainedTermData(TermLike constrainedTermData) {
        this.state = this.state.copy(constrainedTermData);
    }

    @Override
    public ASTNode accept(Transformer transformer) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void accept(Visitor visitor) {
        throw new UnsupportedOperationException();
    }

    public State state() {
        return state;
    }
    
    public TermContext copy() {
        return new TermContext(state);
    }
    
    public TermContext copy(TermLike t) {
        return new TermContext(state.copy(t));
    }
}
