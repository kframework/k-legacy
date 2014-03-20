package org.kframework.backend.pdmc.pda.buchi;

import com.google.common.base.Joiner;
import org.apache.commons.lang3.tuple.Pair;
import org.kframework.backend.pdmc.pda.*;

import java.util.*;

/**
 * @author Traian
 */
//DONE toString method for BuchiPushdownSystem
//DONE Test Product is constructed as supposed
//DONE Test Post* algorithm for product
//DONE Test Post* algorithm for PDS
//DONE toString method for graph
//DONE Compute head reachability graph
//DONE Test head reachibility graph
//DONE Test TarjanSCC
//
//TODO Counterexample generation?
//TODO Integration with K
public class BuchiPushdownSystem<Control, Alphabet>
        implements BuchiPushdownSystemInterface<Control, Alphabet> {
    private PushdownSystemInterface<Control, Alphabet> pds;
    private PromelaBuchi ba;
    private Evaluator<ConfigurationHead<Control, Alphabet>> atomEvaluator;
    Collection<Rule<Pair<Control, BuchiState>, Alphabet>> rules= null;

    public BuchiPushdownSystem(PushdownSystemInterface<Control, Alphabet> pds,
                               PromelaBuchi ba,
                               Evaluator<ConfigurationHead<Control, Alphabet>> atomEvaluator) {
        this.pds = pds;
        this.ba = ba;
        this.atomEvaluator = atomEvaluator;
    }

    /**
     * Computes the initial configuration for this BPDS, whose state is obtained as a pair between the initial
     * state of the PDS and the initial state of the Buchi Automaton.
     * @return the initial configuration for this BPDS.
     */
    @Override
    public Configuration<Pair<Control, BuchiState>, Alphabet> initialConfiguration() {
        BuchiState buchiInitial = ba.initialState();
        Configuration<Control, Alphabet> pdsInitial = pds.initialConfiguration();
        ConfigurationHead<Control, Alphabet> pdsHead = pdsInitial.getHead();

        ConfigurationHead<Pair<Control, BuchiState>, Alphabet> initialHead =
                ConfigurationHead.of(Pair.of(pdsHead.getState(), buchiInitial),
                        pdsHead.getLetter());
        return new Configuration<>(initialHead, pdsInitial.getStack());
    }

    /**
     * Computes the rules associated with this BPDS, following the automaton product definition:
     * <ol>
     *     <li> The set of Buchi Automaton states reachable from current state is computed
     *     (using the {@see Evaluator} object to interpret atoms</li>
     *     <li> Rules of the PDS reachable from the current state are enhanced with the
     *     current Buchi state on the lhs and each of the reachable Buchi states on the rhs.</li>
     * </ol>
     * @param configurationHead the head of configuration for which rules are requested
     * @return the rules of the BPDS having as head configurationHead
     */
    @Override
    public Set<Rule<Pair<Control, BuchiState>, Alphabet>> getRules(
            ConfigurationHead<Pair<Control, BuchiState>, Alphabet> configurationHead) {
        ConfigurationHead<Control, Alphabet> pdsConfigurationHead = ConfigurationHead.of(
                configurationHead.getState().getLeft(), configurationHead.getLetter());
        BuchiState buchiState = configurationHead.getState().getRight();
        atomEvaluator.setState(pdsConfigurationHead);
        Set<BuchiState> transitions = ba.getTransitions(buchiState, atomEvaluator);
        Set<Rule<Control, Alphabet>> pdsRules = pds.getRules(pdsConfigurationHead);
        Set<Rule<Pair<Control, BuchiState>, Alphabet>> rules =
                new HashSet<>(pdsRules.size());
        for (Rule<Control, Alphabet> pdsRule : pdsRules) {
            for ( BuchiState buchiEndState : transitions) {
                Configuration<Control, Alphabet> pdsEndConfig = pdsRule.endConfiguration();
                ConfigurationHead<Control, Alphabet> pdsEndConfigHead = pdsEndConfig.getHead();
                Pair<Control, BuchiState> endState = Pair.of(pdsEndConfigHead.getState(), buchiEndState);
                ConfigurationHead<Pair<Control, BuchiState>, Alphabet> endHead =
                        ConfigurationHead.of(endState,
                                pdsEndConfigHead.getLetter());
                Configuration<Pair<Control, BuchiState>, Alphabet> endConfiguration =
                        new Configuration<>(endHead, pdsEndConfig.getStack());
                rules.add(new Rule<>(configurationHead, endConfiguration));
            }
        }
        return rules;
    }

    @Override
    public boolean isFinal(Pair<Control, BuchiState> state) {
        return state.getRight().isFinal();
    }


    /**
     * Computes an over-approximation of all rules reachable from the initial configuration of this BPDS.
     * It is an over-approximation, as all states and all letters discovered in the process are considered
     * as potential configuration heads.
     * @return a collection of rules containing all rules reachable from the initial configuration
     */
    public Collection<Rule<Pair<Control, BuchiState>, Alphabet>> getRules() {
        if (rules != null) {
            return rules;
        }
        rules = new ArrayList<>();
        Configuration<Pair<Control, BuchiState>, Alphabet> cfg = initialConfiguration();
        Set<Pair<Control, BuchiState>> states = new HashSet<>(); // tracks all states reachable
        Set<Alphabet> letters = new HashSet<>(); //tracks all letters reachable
        // Depth-first search, using states and letters sets to track already considered configurations.
        Stack<ConfigurationHead<Pair<Control, BuchiState>, Alphabet> > toBeProcessed = new Stack<>();
        toBeProcessed.push(cfg.getHead());
        states.add(cfg.getHead().getState());
        letters.add(cfg.getHead().getLetter());
        while (!toBeProcessed.empty()) {
            ConfigurationHead<Pair<Control, BuchiState>, Alphabet> head = toBeProcessed.pop();
            Set<Rule<Pair<Control, BuchiState>, Alphabet>> headRules = getRules(head);
            rules.addAll(headRules);

            for (Rule<Pair<Control, BuchiState>, Alphabet> rule : headRules) {
                cfg = rule.endConfiguration();
                Stack<Alphabet> newstack = new Stack<>();
                newstack.addAll(rule.endStack());
                for (Alphabet l : newstack) { // consider pairs of all states and the newly discovered letters
                    if (!letters.contains(l)) {
                        letters.add(l);
                        for (Pair<Control, BuchiState> state : states) {
                            toBeProcessed.push(ConfigurationHead.of(state, l));
                        }
                    }
                }
                Pair<Control, BuchiState> state = cfg.getHead().getState();
                if (!states.contains(state)) {
                    states.add(state);
                    // consider pairs of a new state with all existing letters.
                    for (Alphabet l : letters) {
                        toBeProcessed.push(ConfigurationHead.of(state, l));
                    }
                }
            }
        }
        return rules;

    }


    @Override
    public String toString() {
        getRules();

        Configuration<Pair<Control, BuchiState>, Alphabet> cfg = initialConfiguration();
        StringBuilder result = new StringBuilder();
        result.append("Initial Configuration: ");
        result.append(cfg.toString());
        result.append("\n");
        Joiner joiner = Joiner.on(";\n");
        joiner.appendTo(result, rules);
        result.append("\n");
        return result.toString();
    }
}
