package org.kframework.backend.pdmc.pda.buchi;

import org.apache.commons.lang3.tuple.Pair;
import org.kframework.backend.pdmc.automaton.Transition;
import org.kframework.backend.pdmc.automaton.TransitionIndex;
import org.kframework.backend.pdmc.pda.Configuration;
import org.kframework.backend.pdmc.pda.ConfigurationHead;
import org.kframework.backend.pdmc.pda.Rule;
import org.kframework.backend.pdmc.pda.graph.TarjanSCC;
import org.kframework.backend.pdmc.pda.pautomaton.PAutomaton;
import org.kframework.backend.pdmc.pda.pautomaton.PAutomatonState;
import org.kframework.backend.pdmc.pda.pautomaton.util.IndexedTransitions;

import java.util.*;

/**
 * Implements some pushdown model-checking related algorithms from  Stefan Schwoon's Phd Thesis:
 * S. Schwoon.  Model-Checking Pushdown Systems.  Ph.D. Thesis, Technische Universitaet Muenchen, June 2002.
 *
 * @author TraianSF
 */
public class BuchiPushdownSystemTools<Control, Alphabet> {

    /**
     * Implements labeled transitions for the Post*  reachability automate.
     * Labels are added on the letters as specified in Schwoon's thesis,
     * Sec. 3.1.6 Witness generation.
     *
     * @param <Control>  specifies the control state of a pushdwown system
     * @param <Alphabet> specifies the alphabet of a pushdown system
     */
    static class LabelledAlphabet<Control, Alphabet> {

        Alphabet letter;
        /**
         * whether this transition passes through an accepting state of the Buchi Automaton
         */
        boolean repeated;

        public Rule<Pair<Control, BuchiState>, Alphabet> getRule() {
            return rule;
        }

        public void setRule(Rule<Pair<Control, BuchiState>, Alphabet> rule) {
            this.rule = rule;
        }

        public PAutomatonState<Pair<Control, BuchiState>, Alphabet> getBackState() {
            return backState;
        }

        public void setBackState(PAutomatonState<Pair<Control, BuchiState>, Alphabet> backState) {
            this.backState = backState;
        }

        /**
         * the rule used to label this transition (use for witness generation)
         */
        Rule<Pair<Control, BuchiState>, Alphabet> rule;
        /**
         * if the transition is due to an epsilon transition identify the intermediate state.
         */
        PAutomatonState<Pair<Control, BuchiState>, Alphabet> backState;

        LabelledAlphabet(Alphabet letter, boolean repeated) {
            this.letter = letter;
            this.repeated = repeated;
            rule = null;
            backState = null;
        }

        public static<Control, Alphabet> LabelledAlphabet<Control, Alphabet> of(Alphabet letter, boolean repeated) {
           return new LabelledAlphabet<>(letter, repeated);
        }

        public Alphabet getLetter() {
            return letter;
        }

        public boolean isRepeated() {
            return repeated;
        }


        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            LabelledAlphabet that = (LabelledAlphabet) o;

            if (repeated != that.repeated) return false;
            if (letter != null ? !letter.equals(that.letter) : that.letter != null) return false;

            return true;
        }

        @Override
        public int hashCode() {
            int result = letter != null ? letter.hashCode() : 0;
            result = 31 * result + (repeated ? 1 : 0);
            return result;
        }

        @Override
        public String toString() {
            return "<" +
                    "" + letter +
                    ", " + repeated +
                    '>';
        }

        public void update(LabelledAlphabet<Control, Alphabet> newLabel) {
            repeated |= newLabel.repeated;
        }
    }

    BuchiPushdownSystemInterface<Control, Alphabet> bps;

    public BuchiPushdownSystemTools(BuchiPushdownSystemInterface<Control, Alphabet> bps) {
        this.bps = bps;
    }

    private PAutomaton<PAutomatonState<Pair<Control, BuchiState>, Alphabet>, Alphabet> postStar = null;
    TarjanSCC<ConfigurationHead<Pair<Control, BuchiState>, Alphabet>,
            LabelledAlphabet<Control, Alphabet>> repeatedHeadsGraph = null;
    Map<Transition<PAutomatonState<Pair<Control, BuchiState>, Alphabet>, Alphabet>, LabelledAlphabet<Control, Alphabet>>
            transitionLabels = null;

    TarjanSCC<ConfigurationHead<Pair<Control, BuchiState>, Alphabet>, LabelledAlphabet<Control, Alphabet>> counterExample = null;

    /**
     * Computes (if not already computed) and returns the <b>post</b> automaton corresponding to this BPDS.
     * <ul>
     *    <li>
     *        The states of this automaton include all states of the BPDS reachable from the initial configuration;
     *    </li>
     *    <li>
     *        Any path q -w->* f from a BPDS state to a final state encodes the reachable configuration (q,w).
     *    </li>
     * </ul>
     * @return the value of the {@code postStar} field after computing it.
     */
    public PAutomaton<PAutomatonState<Pair<Control, BuchiState>, Alphabet>, Alphabet> getPostStar() {
        if (postStar == null)
            compute();
        return postStar;
    }

    /**
     * Computes (if not already computed) and returns the repeated heads graph of the BPDS.
     * This graph is comprised of:
     * <ul>
     *     <li> Vertices represent <b>all</b> configuration heads reachable from the original configuration</li>
     *     <li> Edges indicate reachability (using rules of the BPDS) between configuration heads  </li>
     *     <li> Labels contain: <ul>
     *          <li> information about passing through final states of the Buchi Automaton</li>
     *          <li> </li>
     *     </ul></li>
     * </ul>
     * This is represented as a strongly connected component of the * (reachable) repeated heads graph
     * containing at least one final buchi state.
     * @return the value of the {@code counterExample} field after computing it.
     */
    public TarjanSCC<ConfigurationHead<Pair<Control, BuchiState>, Alphabet>, LabelledAlphabet<Control, Alphabet>> getRepeatedHeadsGraph() {
        if (repeatedHeadsGraph == null)
            compute();
        return repeatedHeadsGraph;
    }

    /**
     * Computes (if not already computed) and returns a witness of an accepting run of the BPDS.
     * This is represented as a strongly connected component of the * (reachable) repeated heads graph
     * containing at least one final buchi state.
     * @return the value of the {@code counterExample} field after computing it.
     */
    public TarjanSCC<ConfigurationHead<Pair<Control, BuchiState>, Alphabet>, LabelledAlphabet<Control, Alphabet>> getCounterExampleGraph() {
        if (repeatedHeadsGraph == null)
            compute();
        return counterExample;
    }

    /**
     * Main method of the class. Implements the post* algorithm instrumented to also produce the repeated heads graph.
     * The post* algorithm implemented is presented in Figure 3.4, Section 3.1.4 of S. Schwoon's PhD thesis (p. 48)
     * The modification to compute the repeated heads graph is explained in Section 3.2.3 of Schwoon's thesis
     * (see also Algorithm 4 in Figure 3.9, p. 81)
     */
    private void compute() {
        transitionLabels = new HashMap<>();
        Set<Transition<PAutomatonState<Pair<Control, BuchiState>, Alphabet>, Alphabet>> trans =
                new HashSet<>();
        IndexedTransitions<PAutomatonState<Pair<Control, BuchiState>, Alphabet>, Alphabet> rel =
                new IndexedTransitions<>();

        Configuration<Pair<Control, BuchiState>, Alphabet> initial = bps.initialConfiguration();
        ConfigurationHead<Pair<Control, BuchiState>, Alphabet> initialHead = initial.getHead();
        PAutomatonState<Pair<Control, BuchiState>, Alphabet> initialState =
                PAutomatonState.of(initialHead.getState());
        assert initial.getStack().isEmpty() : "Only one element in the initial stack accepted at the moment";
        PAutomatonState<Pair<Control, BuchiState>, Alphabet> finalState =
                PAutomatonState.of(initialHead.getState(), initialHead.getLetter());
        Transition<PAutomatonState<Pair<Control, BuchiState>, Alphabet>, Alphabet> transition1 =
                Transition.of(initialState, initialHead.getLetter(), finalState);
        trans.add(transition1);
        LabelledAlphabet<Control, Alphabet> newLabel = LabelledAlphabet.of(initialHead.getLetter(), false);
        updateLabel(transition1, newLabel);
        LabelledAlphabet<Control, Alphabet> labelledLetter;

        while (!trans.isEmpty()) {
            Iterator<Transition<PAutomatonState<Pair<Control, BuchiState>, Alphabet>, Alphabet>> iterator
                    = trans.iterator();
            Transition<PAutomatonState<Pair<Control, BuchiState>, Alphabet>, Alphabet> transition = iterator.next();
            iterator.remove();
            Alphabet gamma = transition.getLetter();
            LabelledAlphabet<Control, Alphabet> oldLabel = transitionLabels.get(transition);
            assert oldLabel != null : "Each transition must have a label";
            if (!rel.contains(transition)) {
                Transition<PAutomatonState<Pair<Control, BuchiState>, Alphabet>, Alphabet> newTransition;
                rel.add(transition);
                boolean b = oldLabel.isRepeated();
                PAutomatonState<Pair<Control, BuchiState>, Alphabet> tp = transition.getStart();
                PAutomatonState<Pair<Control, BuchiState>, Alphabet> q = transition.getEnd();
                if (gamma != null) {
                    assert tp.getLetter() == null : "Expecting PDS state on the lhs of " + transition;
                    Pair<Control, BuchiState> p = tp.getState();
                    final ConfigurationHead<Pair<Control, BuchiState>, Alphabet> configurationHead
                            = ConfigurationHead.of(p, gamma);
                    Set<Rule<Pair<Control, BuchiState>, Alphabet>> rules =
                            bps.getRules(configurationHead);
                    for (Rule<Pair<Control, BuchiState>, Alphabet> rule : rules) {
                        Pair<Control, BuchiState> pPrime = rule.endState();
                        Stack<Alphabet> stack = rule.endStack();
                        assert stack.size() <= 2 : "At most 2 elements are allowed in the stack for now";
                        Alphabet gamma1, gamma2;
                        switch (stack.size()) {
                            case 0:
                                labelledLetter = LabelledAlphabet.of(null, b || bps.isFinal(pPrime));
                                labelledLetter.setRule(rule);
                                newTransition = Transition.of(PAutomatonState.<Pair<Control, BuchiState>, Alphabet>of(pPrime),
                                        null, q);
                                trans.add(newTransition);
                                updateLabel(newTransition, labelledLetter);
                                break;
                            case 1:
                                gamma1 = stack.peek();
                                labelledLetter = LabelledAlphabet.of(gamma1,
                                        b || bps.isFinal(pPrime));
                                labelledLetter.setRule(rule);
                                newTransition = Transition.of(
                                        PAutomatonState.<Pair<Control, BuchiState>, Alphabet>of(pPrime),
                                        gamma1, q);
                                trans.add(newTransition);
                                updateLabel(newTransition, labelledLetter);
                                break;
                            case 2:
                                gamma1 = stack.get(1);
                                gamma2 = stack.get(0);
                                PAutomatonState<Pair<Control, BuchiState>, Alphabet> qPPrimeGamma1
                                        = PAutomatonState.of(pPrime, gamma1);
                                labelledLetter = LabelledAlphabet.of(gamma1,
                                        b || bps.isFinal(pPrime));
                                labelledLetter.setRule(rule);
                                newTransition = Transition.of(
                                        PAutomatonState.<Pair<Control, BuchiState>, Alphabet>of(pPrime),
                                        gamma1, qPPrimeGamma1);
                                trans.add(newTransition);
                                updateLabel(newTransition, labelledLetter);
                                labelledLetter = LabelledAlphabet.of(gamma2, false);
                                labelledLetter.setRule(rule);
                                newTransition = Transition.of(qPPrimeGamma1, gamma2, q);
                                rel.add(newTransition);
                                updateLabel(newTransition, labelledLetter);
                                for (Transition<PAutomatonState<Pair<Control, BuchiState>, Alphabet>, Alphabet> t
                                        : rel.getBackEpsilonTransitions(qPPrimeGamma1)) {
                                    oldLabel = transitionLabels.get(t);
                                    labelledLetter = LabelledAlphabet.of(gamma2, oldLabel.isRepeated());
                                    labelledLetter.setRule(rule);
                                    labelledLetter.setBackState(qPPrimeGamma1);
                                    newTransition = Transition.of(t.getStart(), gamma2, q);
                                    trans.add(newTransition);
                                    updateLabel(newTransition, labelledLetter);
                                }
                        }
                    }
                } else {
                    for (Transition<PAutomatonState<Pair<Control, BuchiState>, Alphabet>, Alphabet> t
                            : rel.getFrontTransitions(q)) {
                        LabelledAlphabet<Control, Alphabet> tLetter = transitionLabels.get(t);
                        labelledLetter = LabelledAlphabet.of(
                                tLetter.getLetter(),
                                tLetter.isRepeated() || b);
                        labelledLetter.setBackState(q);
                        newTransition = Transition.of(tp, t.getLetter(), t.getEnd());
                        trans.add(newTransition);
                        updateLabel(newTransition, labelledLetter);
                    }
                }
            }
        }

        postStar = new PAutomaton<>(
                rel.getTransitions(),
                initialState,
                Collections.singleton(finalState));

        repeatedHeadsGraph = new TarjanSCC<>();

        Collection<ConfigurationHead<Pair<Control, BuchiState>, Alphabet>> reachableHeads = getReachableHeads(postStar);

        for (ConfigurationHead<Pair<Control, BuchiState>, Alphabet> head : reachableHeads) {
            for (Rule<Pair<Control,BuchiState>,Alphabet> rule : bps.getRules(head)) {
                ConfigurationHead<Pair<Control, BuchiState>, Alphabet> endHead = rule.endConfiguration().getHead();
                if (!endHead.isProper()) continue;
                LabelledAlphabet<Control, Alphabet> label = new LabelledAlphabet<>(head.getLetter(), bps.isFinal(head.getState()));
                label.setRule(rule);
                repeatedHeadsGraph.addEdge(head, endHead, label);
                Stack<Alphabet> endStack = rule.endStack();
                if (endStack.size() == 2) {
                    Alphabet gamma1 = endStack.get(1);
                    Alphabet gamma2 = endStack.get(0);
                    PAutomatonState<Pair<Control, BuchiState>, Alphabet> qPPrimeGamma1
                            = PAutomatonState.of(endHead.getState(), gamma1);
                    for (Transition<PAutomatonState<Pair<Control, BuchiState>, Alphabet>, Alphabet> t
                            : rel.getBackEpsilonTransitions(qPPrimeGamma1)) {
                        LabelledAlphabet<Control, Alphabet> oldLabel = transitionLabels.get(t);
                        labelledLetter = new LabelledAlphabet<>(gamma2, oldLabel.isRepeated());
                        labelledLetter.setRule(rule);
                        labelledLetter.setBackState(qPPrimeGamma1);
                        ConfigurationHead<Pair<Control, BuchiState>, Alphabet> endV =
                                ConfigurationHead.of(t.getStart().getState(), gamma2);
                        repeatedHeadsGraph.addEdge(head, endV, labelledLetter);
                    }
                }
            }
        }

        computeCounterExample();

    }

    private Collection<ConfigurationHead<Pair<Control, BuchiState>, Alphabet>> getReachableHeads(PAutomaton<PAutomatonState<Pair<Control, BuchiState>, Alphabet>, Alphabet> postStar) {
        Collection<ConfigurationHead<Pair<Control, BuchiState>, Alphabet>> heads = new ArrayList<>();
        for (TransitionIndex<PAutomatonState<Pair<Control, BuchiState>, Alphabet>, Alphabet> index : postStar.getDeltaIndex().keySet()) {
            PAutomatonState<Pair<Control, BuchiState>, Alphabet> pState = index.getState();
            if (pState.getLetter() == null) {
                heads.add(ConfigurationHead.of(pState.getState(), index.getLetter()));
            }
        }
        return heads;

    }

    private LabelledAlphabet<Control, Alphabet> updateLabel(Transition<PAutomatonState<Pair<Control, BuchiState>, Alphabet>, Alphabet> transition, LabelledAlphabet<Control, Alphabet> label) {
        LabelledAlphabet<Control, Alphabet> labelledLetter = transitionLabels.get(transition);
        if (labelledLetter == null) {
            transitionLabels.put(transition, label);
            labelledLetter = label;
        } else {
            labelledLetter.update(label);
        }
        transition.setLabel(labelledLetter);
        return labelledLetter;
    }

    private void computeCounterExample() {
        Collection<Collection<TarjanSCC<ConfigurationHead<Pair<Control, BuchiState>, Alphabet>, LabelledAlphabet<Control, Alphabet>>.TarjanSCCVertex>> sccs = repeatedHeadsGraph.getStronglyConnectedComponents();
        for (Collection<TarjanSCC<ConfigurationHead<Pair<Control, BuchiState>, Alphabet>, LabelledAlphabet<Control, Alphabet>>.TarjanSCCVertex> scc : sccs) {
            TarjanSCC<ConfigurationHead<Pair<Control, BuchiState>, Alphabet>, LabelledAlphabet<Control, Alphabet>> sccSubGraph = repeatedHeadsGraph.getSubgraph(scc);
            for (Map<ConfigurationHead<Pair<Control, BuchiState>, Alphabet>, LabelledAlphabet<Control, Alphabet>> values : sccSubGraph.getEdgeSet().values()) {
                for (LabelledAlphabet<Control, Alphabet> label: values.values()) {
                    if (label.isRepeated()) {
                        counterExample = sccSubGraph;
                        return;
                    }
                }
            }
        }
    }

    /**
     * Implements Witness generation algorithm from Schwoon's thesis, Section 3.1.6
     * @param head A reachable configuration head
     * @return The path (of rules) from the initial configuraiton to {@code head}
     */
    public Deque<Rule<Pair<Control, BuchiState>, Alphabet>> getReachableConfiguration(
           ConfigurationHead<Pair<Control, BuchiState>, Alphabet> head
    ) {
        Deque<Rule<Pair<Control, BuchiState>, Alphabet>> result = new ArrayDeque<>();
        //Step 1
        Deque<Transition<PAutomatonState<Pair<Control, BuchiState>, Alphabet>, Alphabet>> path = postStar.getPath(
                PAutomatonState.<Pair<Control, BuchiState>, Alphabet>of(head.getState()),
                head.getLetter(),
                postStar.getFinalStates().iterator().next());
        //Step 2
        Transition<PAutomatonState<Pair<Control, BuchiState>, Alphabet>, Alphabet> transition = path.removeFirst();
        LabelledAlphabet<Control, Alphabet> label = transitionLabels.get(transition);
        Rule<Pair<Control, BuchiState>, Alphabet> rule = label.getRule();
        PAutomatonState<Pair<Control, BuchiState>, Alphabet> backState;
        while (rule != null) {
            if (rule.endStack().size() == 2) { // reduce step 3.2 to step 3.1 by shifting transition & rules
                transition = path.removeFirst();
                label = transitionLabels.get(transition);
                rule = label.getRule();
                assert rule != null && rule.endStack().size() == 2;
            }
            // Step 3.1
            result.addFirst(rule);
            head = rule.getHead();
            backState = label.getBackState();
            if (backState == null) {
                transition = Transition.of(
                        PAutomatonState.<Pair<Control, BuchiState>, Alphabet>of(head.getState()),
                        head.getLetter(),
                        transition.getEnd()
                );
            } else {
                transition = Transition.of(
                        backState,
                        head.getLetter(),
                        transition.getEnd()
                );
                path.addFirst(transition);
                transition = Transition.of(
                        PAutomatonState.<Pair<Control, BuchiState>, Alphabet>of(head.getState()),
                        null,
                        backState
                );
            }

            label = transitionLabels.get(transition);
            rule = label.getRule();
        }
        return result;
    }

}
