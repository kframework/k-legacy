// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.debugger;


import org.jgrapht.DirectedGraph;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.kframework.Kapi;
import org.kframework.RewriterResult;
import org.kframework.attributes.Source;
import org.kframework.builtin.BooleanUtils;
import org.kframework.builtin.KLabels;
import org.kframework.definition.Rule;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kore.Assoc;
import org.kframework.kore.K;
import org.kframework.kore.KApply;
import org.kframework.kore.KORE;
import org.kframework.krun.KRun;
import org.kframework.krun.KRunOptions;
import org.kframework.rewriter.Rewriter;
import org.kframework.rewriter.SearchType;
import org.kframework.utils.Goal;
import org.kframework.utils.PatternNode;
import org.kframework.utils.ProofTransition;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Optional;
import java.util.TreeMap;
import java.util.stream.Collectors;

/**
 * Kore Based Debugger Implementation.
 */
public class KoreKDebug implements KDebug {

    private final int DEFAULT_ID = 0;
    private List<DebuggerState> stateList;
    private int activeStateIndex;
    private Rewriter rewriter;
    private int checkpointInterval;
    private FileUtil files;
    private final KExceptionManager kem;
    private KRunOptions options;
    private CompiledDefinition compiledDef;
    private DirectedGraph<PatternNode, ProofTransition> proofTree;
    private List<Goal> goalsList;
    private final int GOALS_ABSENT_ID = -1;
    private final int DEFAULT_GOAL_ID = 0;
    private int activeGoalId;

    /**
     * Start a Debugger Session. The initial Configuration becomes a part of the new and only state of the Debugger
     *
     * @param initialK The initial Configuration.
     * @param rewriter The Rewriter being used.
     */
    public KoreKDebug(K initialK,
                      Rewriter rewriter,
                      int checkpointInterval,
                      FileUtil files,
                      KExceptionManager kem,
                      KRunOptions options,
                      CompiledDefinition compiledDef) {
        this.stateList = new ArrayList<>();
        this.rewriter = rewriter;
        this.checkpointInterval = checkpointInterval;
        this.files = files;
        this.kem = kem;
        this.options = options;
        this.compiledDef = compiledDef;
        this.proofTree = new DefaultDirectedGraph<>(ProofTransition.class);
        NavigableMap<Integer, K> checkpointMap = new TreeMap<>();
        List<DebuggerMatchResult> watchList = new ArrayList<>();
        activeStateIndex = -1;
        if (initialK != null) {
            checkpointMap.put(DEFAULT_ID, initialK);
            DebuggerState initialState = new DebuggerState(initialK, DEFAULT_ID, checkpointMap, watchList);
            stateList.add(initialState);
            activeStateIndex = DEFAULT_ID;
        }
        goalsList = new ArrayList<>();
        this.activeGoalId = GOALS_ABSENT_ID;
    }

    @Override
    public void setCheckpointInterval(int checkpointInterval) {
        this.checkpointInterval = checkpointInterval;
    }

    @Override
    public DebuggerState step(int currentStateIndex, int steps) {
        if (currentStateIndex == -1) {
            return null;
        }
        DebuggerState currentState = stateList.get(currentStateIndex);
        K currentK = currentState.getCurrentK();
        int activeStateCheckpoint = currentState.getStepNum();
        RewriterResult result;
        NavigableMap<Integer, K> checkpointMap = new TreeMap<>(currentState.getCheckpointMap());
        List<DebuggerMatchResult> origWatchList = currentState.getWatchList();
        while (steps >= checkpointInterval) {
            result = rewriter.execute(currentK, Optional.of(checkpointInterval));
            if (isFinalState(checkpointInterval, result)) {
                return processStateData(result.k(),
                        activeStateCheckpoint + result.rewriteSteps().get(),
                        currentStateIndex,
                        checkpointMap,
                        origWatchList
                );
            }
            steps -= checkpointInterval;
            activeStateCheckpoint += checkpointInterval;
            checkpointMap.put(activeStateCheckpoint, result.k());
            currentK = result.k();
        }
        result = rewriter.execute(currentK, Optional.of(steps));
        if (isFinalState(steps, result)) {
            return processStateData(result.k(),
                    activeStateCheckpoint + result.rewriteSteps().get(),
                    currentStateIndex,
                    checkpointMap,
                    origWatchList
            );

        }
        activeStateCheckpoint += steps;
        return processStateData(result.k(),
                activeStateCheckpoint,
                currentStateIndex,
                checkpointMap,
                origWatchList
        );
    }

    /* Private Helper function to do make a new state with the requested data */
    private DebuggerState processStateData(K finalK, int stepNum, int stateNum, NavigableMap<Integer, K> checkpointMap, List<DebuggerMatchResult> watchList) {
        List<DebuggerMatchResult> updatedWatchList = updateWatchList(watchList, finalK);
        stateList.remove(stateNum);
        DebuggerState nextState = new DebuggerState(
                finalK,
                stepNum,
                checkpointMap,
                updatedWatchList
        );
        stateList.add(stateNum, nextState);
        return nextState;
    }

    private List<DebuggerMatchResult> updateWatchList(List<DebuggerMatchResult> originalList, K finalK) {
        List<DebuggerMatchResult> modifiableList = new ArrayList<>(originalList);
        return modifiableList.stream()
                .map(x -> {
                    return new DebuggerMatchResult(
                            rewriter.match(finalK, x.getCompiledRule()),
                            x.getParsedRule(),
                            x.getCompiledRule(),
                            x.getPattern());
                })
                .collect(Collectors.toCollection(ArrayList::new));
    }

    private boolean isFinalState(int steps, RewriterResult result) {
        return result.rewriteSteps().isPresent() && result.rewriteSteps().get() < steps;
    }

    private boolean isFinalConfiguration(K config) {
        return rewriter.execute(config, Optional.of(1)).equals(config);
    }

    @Override
    public DebuggerState backStep(int initialStateNum, int steps) {
        DebuggerState currentState = stateList.get(initialStateNum);
        int currentCheckpoint = currentState.getStepNum();
        int target = currentCheckpoint - steps;
        NavigableMap<Integer, K> currMap = new TreeMap<>(currentState.getCheckpointMap());
        Map.Entry<Integer, K> relevantEntry = currMap.floorEntry(target);
        if (relevantEntry == null) {
            /* Invalid Operation, no need to change the state */
            return null;
        }

        int floorKey = relevantEntry.getKey();
        processStateData(relevantEntry.getValue(),
                floorKey,
                initialStateNum,
                currMap.headMap(floorKey, true),
                updateWatchList(currentState.getWatchList(), relevantEntry.getValue()));
        return step(initialStateNum, target - floorKey);
    }

    @Override
    public DebuggerState jumpTo(int initialStateNum, int configurationNum) {
        DebuggerState currentState = stateList.get(initialStateNum);
        NavigableMap<Integer, K> checkpointMap = new TreeMap<>(currentState.getCheckpointMap());
        int firstKey = checkpointMap.firstKey();
        if (configurationNum < firstKey) {
            return null;
        }
        int lastKey = currentState.getStepNum();
        if (configurationNum >= lastKey) {
            return step(initialStateNum, configurationNum - lastKey);
        }
        return backStep(initialStateNum, lastKey - configurationNum);
    }

    @Override
    public K search(Rule searchPattern, Optional<Integer> depth, Optional<Integer> bounds) {
        return rewriter.search(stateList.get(activeStateIndex).getCurrentK(), depth, bounds, searchPattern, SearchType.FINAL, true);
    }

    @Override
    public DebuggerState resume() {
        DebuggerState activeState = stateList.get(activeStateIndex);
        DebuggerState steppedState = activeState;
        do {
            activeState = steppedState;
            steppedState = step(activeStateIndex, checkpointInterval);
        }
        while (steppedState.getStepNum() - activeState.getStepNum() >= checkpointInterval && !isFinalConfiguration(steppedState.getCurrentK()));
        return steppedState;
    }

    @Override
    public List<DebuggerState> getStates() {
        return new ArrayList<>(stateList);
    }


    @Override
    public DebuggerState setState(int stateNum) {
        if (stateNum > stateList.size() - 1) {
            return null;
        }
        DebuggerState newActiveState = stateList.get(stateNum);
        if (newActiveState == null) {
            return null;
        }
        activeStateIndex = stateNum;
        return newActiveState;
    }

    @Override
    public int getActiveStateId() {
        return activeStateIndex;
    }

    @Override
    public DebuggerState createCopy(int stateNum) {
        if (stateList.size() <= stateNum || stateNum < 0) {
            return null;
        }
        DebuggerState newState = new DebuggerState(stateList.get(stateNum));
        stateList.add(stateList.size() - 1, newState);
        return newState;
    }

    @Override
    public DebuggerState getActiveState() {
        return stateList.get(activeStateIndex);
    }

    @Override
    public DebuggerMatchResult match(String pattern, String source) {
        String DebuggerSource = source;
        Rule compiledPattern = KRun.compilePattern(files, kem, pattern, options, compiledDef, Source.apply(DebuggerSource));
        Rule parsedPattern = KRun.parsePattern(files, kem, pattern, compiledDef, Source.apply(DebuggerSource));
        K subst = rewriter.match(getActiveState().getCurrentK(), compiledPattern);
        return new DebuggerMatchResult(subst, parsedPattern, compiledPattern, pattern);
    }

    @Override
    public void addWatch(String pattern, String watchSource) {
        DebuggerMatchResult matchResult = match(pattern, watchSource);
        DebuggerState activeState = stateList.remove(activeStateIndex);
        List<DebuggerMatchResult> watchList = new ArrayList<>(activeState.getWatchList());
        watchList.add(matchResult);
        DebuggerState nextState = new DebuggerState(
                activeState.getCurrentK(),
                activeState.getStepNum(),
                new TreeMap<>(activeState.getCheckpointMap()),
                watchList);
        stateList.add(activeStateIndex, nextState);
    }

    @Override
    public int removeWatch(int watchNum) {
        DebuggerState currActiveState = stateList.get(activeStateIndex);
        List<DebuggerMatchResult> watchList = currActiveState.getWatchList();
        if (watchNum < 0 || watchNum >= watchList.size()) {
            return -1;
        }
        stateList.remove(currActiveState);
        List<DebuggerMatchResult> updatedList = new ArrayList<>(watchList);
        updatedList.remove(watchNum);
        stateList.add(activeStateIndex,
                new DebuggerState(
                        currActiveState.getCurrentK(),
                        currActiveState.getStepNum(),
                        new TreeMap<>(currActiveState.getCheckpointMap()),
                        updatedList)

        );
        return watchNum;
    }

    @Override
    public ProofState getProofState() {
        return new ProofState(goalsList, activeGoalId);
    }

    @Override
    public ProofState addPatternSourceFile(String filename) {
        List<Rule> result = Kapi.parseAndConcretizePattern(filename, compiledDef);
        result.forEach(x -> goalsList.add(new Goal(x, false)));
        if (activeGoalId == GOALS_ABSENT_ID && !result.isEmpty()) {
            activeGoalId = DEFAULT_GOAL_ID;
        }
        return new ProofState(goalsList, activeGoalId);
    }

    @Override
    public ProofState stepAll(int stepNum) {
        if (activeGoalId == GOALS_ABSENT_ID) {
            return null;
        }
        Goal currentGoal = goalsList.get(activeGoalId);

        DirectedGraph<PatternNode, ProofTransition> graph = currentGoal.getProofTree();
        List<PatternNode> leafNodes = graph.vertexSet().stream().filter(x -> graph.outDegreeOf(x) == 0).collect(Collectors.toList());
        leafNodes.forEach(x -> {
            Rule defaultRule = new Rule(KORE.KVariable("Default"),
                    BooleanUtils.TRUE, BooleanUtils.TRUE, KORE.Att());
            K result = rewriter.search(x.getPattern(), Optional.of(stepNum), Optional.empty(), defaultRule, SearchType.FINAL, false);
            List<K> resultList = disjunctPattern(result);
            resultList.forEach(y -> {
                int nodeId = currentGoal.getNodeIds();
                PatternNode newNode = new PatternNode(y, nodeId++);
                graph.addVertex(newNode);
                graph.addEdge(x, newNode, new ProofTransition(stepNum, "stepAll"));
                currentGoal.setNodeIds(nodeId);
            });
        });
        return new ProofState(goalsList, activeGoalId);
    }

    private List<K> disjunctPattern(K result) {
        List<K> resultList;
        if (result instanceof KApply && ((KApply) result).klabel().equals(KORE.KLabel(KLabels.ML_OR))) {
            resultList = Assoc.flatten(KORE.KLabel(KLabels.ML_OR), ((KApply) result).items(), KORE.KLabel(KLabels.ML_FALSE))
                    .stream().sorted(Comparator.comparing(K::toString)).collect(Collectors.toList());
        } else {
            resultList = new ArrayList<>();
            resultList.add(result);
        }
        return resultList;
    }

    @Override
    public DebuggerState matchUntilPattern(String filename) {
//        K matchPattern = Kapi.parseAndConcretizePattern(filename, compiledDef);
//        if (stateList.isEmpty()) {
//            return null;
//        }
//        K searchResult = rewriter.search(stateList.get(activeStateIndex).getCurrentK(), Optional.empty(), Optional.empty(), Rule.apply(matchPattern, KORE.KApply(KORE.KLabel(KLabels.ML_TRUE)), KORE.KApply(KORE.KLabel(KLabels.ML_TRUE)), matchPattern.att()), SearchType.FINAL, false);
//        return new DebuggerState(searchResult, activeStateIndex + 1, new TreeMap<>(), new ArrayList<>());
        return null;

    }
}
