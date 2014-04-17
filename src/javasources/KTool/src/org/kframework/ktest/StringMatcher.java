// Copyright (c) 2014 K Team. All Rights Reserved.
package org.kframework.ktest;

public interface StringMatcher extends Cloneable {
    /**
     * Checks whether {@code actual} satisfies the constraint specified by {@code pattern}.
     * 
     * An implementation of this interface may choose any meaning it prefers for how to interpret the
     * {@code pattern}. Thus, from the perspective of this interface, a pattern is an opaque reference
     * to a set of strings that, when passed as the parameter {@code actual}, will return true.
     * @param pattern A implementation-specific representation of a set of strings this matcher matches.
     * @param actual The string being matched against the {@code pattern}.
     * @return {@code true} if {@code actual} matches {@code pattern}; {@code false} otherwise.
     */
    boolean matches(String pattern, String actual);
    
    /**
     * Returns a string representation of the reason why the most recent invocation of 
     * {@link #matches(String, String) matches} which returned false failed to match against
     * the pattern.
     * @return
     */
    String errorMessage();
    
    /**
     * Designed to return an equivalent StringMatcher with its internal state reset. Used to ensure that 
     * each program run by ktest is run with a different StringMatcher object. This prevents a race condition
     * where the ordering of tests run on multithreaded machines causes the error message to be incorrect.
     * @return an equivalent StringMatcher without any errors accumulated.
     */
    StringMatcher copy();
}