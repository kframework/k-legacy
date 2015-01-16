// Copyright (c) 2013-2015 K Team. All Rights Reserved.
package org.kframework.krun.api;

import org.kframework.kil.Term;

import com.google.inject.Singleton;

import java.io.Serializable;

public class KRunState implements Serializable, Comparable<KRunState>, KRunResult {

    /**
    The term container associated with this state, as suitable for further rewriting
    */
    private KilTermContainer rawResult;

    /**
     * A state ID corresponding to this state. The contract of a {@link KRun} object
     * demands that no two distinct states have the same ID. However, it does not
     * guarantee the inverse: it is the responsibility of any callers who wish
     * to ensure that the mapping is one-to-one to maintain a cache of states
     * and canonicalize the output of the KRun object.
     */
    private int stateId;

    @Singleton
    public static class Counter {
        private int nextState;
    }

    public KRunState(KilTermContainer rawResult, Counter counter) {
        this.rawResult = rawResult;
        this.stateId = counter.nextState++;
    }

    public Term getRawResult() {
        return rawResult.getKilTerm();
    }

    public KilTermContainer getTermContainer() {
        return rawResult;
    }
    
    public Integer getStateId() {
        return stateId;
    }

    public void setStateId(Integer stateId) {
        this.stateId = stateId;
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof KRunState)) return false;
        KRunState s = (KRunState)o;
        /*jung uses intensively equals while drawing graphs
          use SemanticEquals since it caches results
        */
        //return SemanticEqual.checkEquality(rawResult, s.rawResult);
        return rawResult.equals(s.getTermContainer());
    }

    @Override
    public int hashCode() {
        return rawResult.hashCode();
    }

    @Override
    public int compareTo(KRunState arg0) {
        return Integer.compare(stateId, arg0.stateId);
    }
}
