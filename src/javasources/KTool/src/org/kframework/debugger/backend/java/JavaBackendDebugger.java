package org.kframework.debugger.backend.java;

import edu.uci.ics.jung.graph.DirectedGraph;
import edu.uci.ics.jung.graph.DirectedSparseGraph;
import org.apache.commons.collections15.bidimap.DualHashBidiMap;
import org.kframework.backend.java.symbolic.JavaSymbolicKRun;
import org.kframework.kil.Term;
import org.kframework.krun.KRunExecutionException;
import org.kframework.krun.api.*;
import org.kframework.kil.loader.Context;
import org.mockito.internal.matchers.Null;

public class JavaBackendDebugger implements KRunDebugger {
    private KRun kRun;
    private DirectedGraph<KRunState, Transition> stateGraph;
    private DualHashBidiMap<Integer, KRunState> idMap;
    private int idCounter = 0;
    private int currentId;
    private Context context;
    public JavaBackendDebugger(Term cfg, KRun kRun, Context context){
        this.kRun = kRun;
        this.context = context;
        stateGraph = new DirectedSparseGraph<KRunState, Transition>();
        idMap = new DualHashBidiMap<Integer, KRunState>();
        KRunState initialState = new KRunState(cfg, context);
        //add the initial state to the graph
        stateGraph.addVertex(initialState);
        //add the id to the id-Map
        currentId = idCounter;
        idMap.put(idCounter++, initialState);
    }

    @Override
    public DirectedGraph<KRunState, Transition> getGraph() {
        return stateGraph;
    }

    @Override
    public Integer getCurrentState() {
        return currentId;
    }

    @Override
    public void setCurrentState(Integer stateNum) {
        currentId = stateNum;
    }

    @Override
    public KRunState getState(int stateNum) {
        return idMap.get(stateNum);
    }

    @Override
    public Transition getEdge(int state1, int state2){
        return stateGraph.findEdge(idMap.get(state1), idMap.get(state2));
    }

    @Override
    public void step(int steps) throws KRunExecutionException {
        JavaSymbolicKRun javaKRun = (JavaSymbolicKRun)kRun;
        javaKRun.debugStep((idMap.get(currentId)).getRawResult(), 1);
    }

    @Override
    public SearchResults stepAll(int steps) throws KRunExecutionException {
        return null;
    }

    @Override
    public void resume() throws KRunExecutionException {

    }

    @Override
    public String printState(int stateNum) {
        return idMap.get(currentId).toString();
    }

    @Override
    public String printEdge(int state1, int state2) {
        return null;
    }

    @Override
    public void readFromStdin(String s) {

    }
}
