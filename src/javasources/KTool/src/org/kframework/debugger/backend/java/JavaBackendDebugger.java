package org.kframework.debugger.backend.java;

import edu.uci.ics.jung.graph.DirectedGraph;
import edu.uci.ics.jung.graph.DirectedSparseGraph;
import org.apache.commons.collections15.bidimap.DualHashBidiMap;
import org.kframework.backend.java.kil.Rule;
import org.kframework.backend.java.symbolic.JavaSymbolicKRun;
import org.kframework.compile.utils.CompilerStepDone;
import org.kframework.compile.utils.RuleCompilerSteps;
import org.kframework.kil.*;
import org.kframework.kil.visitors.exceptions.ParseFailedException;
import org.kframework.krun.KRunDebugResult;
import org.kframework.krun.KRunExecutionException;
import org.kframework.krun.KRunOptions;
import org.kframework.krun.api.*;
import org.kframework.kil.loader.Context;
import org.kframework.krun.api.io.File;
import org.kframework.parser.DefinitionLoader;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.inject.Options;
import edu.uci.ics.jung.graph.util.Pair;

import java.util.ArrayList;
import java.util.List;

public class JavaBackendDebugger implements KRunDebugger {
    private KRun kRun;
    private DirectedGraph<KRunState, Transition> stateGraph;
    private DualHashBidiMap<Integer, KRunState> idMap;
    private ArrayList<KRunState> primaryStateList;
    private int idCounter = 0;
    private int currentId;
    private Context context;
    private static org.kframework.kil.Rule defaultPattern;
    private static RuleCompilerSteps defaultPatternInfo;
    public JavaBackendDebugger(Term cfg, KRun kRun, Context context){
        this.kRun = kRun;
        this.context = context;
        stateGraph = new DirectedSparseGraph<KRunState, Transition>();
        idMap = new DualHashBidiMap<Integer, KRunState>();
        primaryStateList = new ArrayList<KRunState>();
        KRunState initialState = new KRunState(cfg, context);
        initialState.setStateId(idCounter);
        //add the initial state to the graph
        stateGraph.addVertex(initialState);
        //add the state to the primary state list
        primaryStateList.add(initialState);
        //add the id to the id-Map
        currentId = idCounter;
        idMap.put(idCounter++, initialState);
        try {
            org.kframework.parser.concrete.KParser.ImportTblRule(context.kompiled);
            ASTNode pattern = DefinitionLoader.parsePattern(
                    KRunOptions.DEFAULT_PATTERN,
                    null,
                    KSorts.BAG,
                    context);
            defaultPatternInfo = new RuleCompilerSteps(context);
            pattern = defaultPatternInfo.compile(new org.kframework.kil.Rule((Sentence) pattern), null);

            defaultPattern = (org.kframework.kil.Rule) pattern;
        } catch (CompilerStepDone e) {
            e.printStackTrace();
        } catch (ParseFailedException e) {
            e.report();
        }

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
        KRunState currentState = getState(getCurrentState());
        boolean primaryState = primaryStateList.contains(currentState);
        int step = 0;
        while(step != steps){
            ++step;
            KRunDebugResult result = javaKRun.debugStep((idMap.get(currentId)).getRawResult(), 1);
            //check if final state is reached
            if(result.getStateList().size() == 0){
                System.out.println("Final State Reached");
                break;
            }
            KRunState resultState = result.getStateList().get(0);
            if(!stateGraph.containsVertex(resultState)){
                resultState.setStateId(idCounter++);
                stateGraph.addVertex(resultState);
                idMap.put(resultState.getStateId(), resultState);
                //if the first state is primary, all following states will be primary in step
                if(primaryState){
                    primaryStateList.add(resultState);
                }
                Transition transition = Transition.rule(result.getRuleList().get(0), context);
                stateGraph.addEdge(transition, currentState, resultState);
                currentState = resultState;
                currentId = resultState.getStateId();
            }
            else {
                //state already exists, add the transition
                if(primaryState){
                    primaryStateList.add(resultState);
                }
                Transition transition = Transition.rule(result.getRuleList().get(0), context);
                stateGraph.addEdge(transition, currentState, resultState);
                currentState = resultState;
                currentId = resultState.getStateId();
            }
        }
    }

    @Override
    public SearchResults stepAll(int steps) throws KRunExecutionException {
/*        JavaSymbolicKRun javaKRun = (JavaSymbolicKRun)kRun;
        boolean primaryState;
        List<SearchResult> searchResults = new ArrayList<>();
        for(int counter = 0; counter < steps; ++counter){
            KRunState currentState = idMap.get(currentId);
            primaryState = primaryStateList.contains(currentState);
            KRunDebugResult stepResult = javaKRun.debugStepAll(currentState.getRawResult(), 1);
            List<KRunState> stateList = stepResult.getStateList();
            List<Rule> ruleList = stepResult.getRuleList();
            for(int index = 0; index < stateList.size(); ++index){
                KRunState stepState = stateList.get(index);
                //first state is important, since kRun follows the first state
                if(!stateGraph.containsVertex(stepState)){
                    stepState.setStateId(idCounter++);
                    idMap.put(stepState.getStateId(), stepState);
                }
                Rule rule = ruleList.get(index);
                Transition transition = Transition.rule(rule, context);
                stateGraph.addEdge(transition, currentState, stepState);
                if(index == 0){
                    //the first step of primary state is primary
                    if(primaryState) {
                        if (!primaryStateList.contains(stepState)) {
                            primaryStateList.add(stepState);
                        }
                    }
                    //the next state for the step, since KRun also does this.
                    currentState = stepState;

                    currentId = currentState.getStateId();
                }
                //adding the state to the search results
                //SearchResult result = new SearchResult(stepState, s)
            }

        }*/
        KRunState currentState = idMap.get(currentId);
        KRunResult<SearchResults> searchResult = kRun.search(null, steps, SearchType.STAR, defaultPattern,
                                                            currentState.getRawResult(), new RuleCompilerSteps(context));
        System.out.println("hello from stepall");
        SearchResults results = searchResult.getResult();
        System.out.println(results.toString());
        DirectedGraph<KRunState, Transition> partialGraph = results.getGraph();
        System.out.println(partialGraph);
        mergeGraph(partialGraph);
        //System.out.println(searchResult);
        return searchResult.getResult();
    }
    public void mergeGraph(DirectedGraph<KRunState, Transition> partialGraph){
        for (KRunState state : partialGraph.getVertices()){
            if(! stateGraph.containsVertex(state)){
                state.setStateId(idCounter++);
                idMap.put(state.getStateId(), state);
                stateGraph.addVertex(state);
            }
        }
        for(Transition edge : partialGraph.getEdges()){
            Pair<KRunState> vertices = partialGraph.getEndpoints(edge);
            Transition originalEdge = stateGraph.findEdge(vertices.getFirst(), vertices.getSecond());
            if (originalEdge != null){
                //update the original edge
                stateGraph.removeEdge(originalEdge);
                stateGraph.addEdge(edge, vertices.getFirst(), vertices.getSecond());
            }
            //add the new edge to the graph
            else{
                stateGraph.addEdge(edge, vertices.getFirst(), vertices.getSecond());
            }
        }
    }
    public DirectedGraph<KRunState, Transition> debugStepAll(int steps){
        JavaSymbolicKRun javaKRun = (JavaSymbolicKRun)kRun;
        boolean primaryState;
        //partial graph is to be returned to the frontend.
        DirectedGraph<KRunState, Transition> partialGraph = new DirectedSparseGraph<KRunState, Transition>();
        for(int counter = 0; counter < steps; ++counter){
            KRunState currentState = idMap.get(currentId);
            partialGraph.addVertex(currentState);
            primaryState = primaryStateList.contains(currentState);
            KRunDebugResult stepResult = javaKRun.debugStepAll(currentState.getRawResult(), 1);
            List<KRunState> stateList = stepResult.getStateList();
            List<Rule> ruleList = stepResult.getRuleList();
            for(int index = 0; index < stateList.size(); ++index){
                KRunState stepState = stateList.get(index);
                partialGraph.addVertex(stepState);
                //first state is important, since kRun follows the first state
                if(!stateGraph.containsVertex(stepState)){
                    stepState.setStateId(idCounter++);
                    idMap.put(stepState.getStateId(), stepState);
                }
                Rule rule = ruleList.get(index);
                Transition transition = Transition.rule(rule, context);
                stateGraph.addEdge(transition, currentState, stepState);
                partialGraph.addEdge(transition, currentState, stepState);
                if(index == 0){
                    //the first step of primary state is primary
                    if(primaryState) {
                        if (!primaryStateList.contains(stepState)) {
                            primaryStateList.add(stepState);
                        }
                    }
                    //the next state for the step, since KRun also does this.
                    currentState = stepState;

                    currentId = currentState.getStateId();
                }
                //adding the state to the search results
                //SearchResult result = new SearchResult(stepState, s)
            }
        }
        return partialGraph;
    }

    @Override
    public void resume() throws KRunExecutionException {
        //step until final state is reached. Change Magic Number later.
        step(-1);
    }

    @Override
    public String printState(int stateNum) {
        return idMap.get(currentId).toString();
    }

    @Override
    public String printEdge(int state1, int state2) {
        if(! idMap.containsKey(state1) || !idMap.containsKey(state2)){
            throw new IllegalArgumentException("State doesn't exist");
        }
        else{
            Transition transition = stateGraph.findEdge(idMap.get(state1), idMap.get(state2));
            if(transition == null){
                throw new IllegalStateException("No edge between these states");
            }
            return processTransition(transition);
        }
    }

    @Override
    public void readFromStdin(String s) {

    }
    private String processTransition(Transition transition){
        ASTNode rule = transition.getRule();
        Location ruleLocation = rule.getLocation();
        Source ruleSource = rule.getSource();
        //String file = FileUtil.getFileContent(ruleSource.toString());
        return rule.toString();
    }
}
