package org.kframework.parser.concrete2;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.kframework.parser.concrete2.Grammar.ExitState;
import org.kframework.parser.concrete2.Grammar.NextableState;
import org.kframework.parser.concrete2.Grammar.NonTerminal;
import org.kframework.parser.concrete2.Grammar.NonTerminalState;
import org.kframework.parser.concrete2.Grammar.PrimitiveState;
import org.kframework.parser.concrete2.Grammar.PrimitiveState.MatchResult;
import org.kframework.parser.concrete2.Grammar.State;

/**
 * Helper class in the parser that finds all of the nullable NonTerminals in a grammar.
 * @author Radu
 *
 */
public class NullabilityCheck {
    /**
     * Starting from the start symbol, find all the non terminals that can match on the empty string.
     * @param startNt The start non terminal.
     * @return A set of all reachable non terminals that are nullable.
     */
    public static Set<NonTerminal> getReachableNullableNT(NonTerminal startNt) {
        NullabilityCheck nc = new NullabilityCheck();
        return nc.checkNullability2(startNt);
    }

    Set<NonTerminal> nullable = new HashSet<>();
    Map<NonTerminal, Set<State>> dependencies = new HashMap<>();
    Set<NonTerminal> all = new HashSet<>();

    public Set<NonTerminal> checkNullability2(NonTerminal startNt) {
        getReachableNT(startNt.entryState, new HashSet<State>());
        all.add(startNt);
        Set<State> visited = new HashSet<>();

        // insert all the non-terminals and their start state
        for (NonTerminal nt : all) {
            dependencies.put(nt, new HashSet<State>(Arrays.asList(nt.entryState)));
            visited.add(nt.entryState);
        }
        
        // mapping each NonTerminal to the states that may lead to an ExitState
        // when I get to an ExitState, remove the NonTerminal from the work list and mark it as nullable.
        // if I get to a PrimitiveState that is not nullable, remove it
        // if I get to a NonTerminalState that not nullable (yet), keep it
        //    else it means I found a PrimitiveState that is nullable or a NonTerminalState that may be nullable
        //         add all of their next elements to the work list.

        boolean changed;
        do {
            changed = false;
            Iterator<Entry<NonTerminal, Set<State>>> iter = dependencies.entrySet().iterator();
            // iterate over each NonTerminal
            while (iter.hasNext()) {
                Entry<NonTerminal, Set<State>> ee = iter.next();
                Set<State> keep = new HashSet<>();
                // iterate over each next state from this NonTerminal
                for (State st : ee.getValue()) {
                    if (st instanceof ExitState) {
                        nullable.add(ee.getKey());
                        iter.remove();
                        changed = true;
                        break;
                    } else { // nextable states
                        assert st instanceof NextableState : "Expected NextableState here!";

                        if (st instanceof PrimitiveState) {
                            PrimitiveState ss = (PrimitiveState) st;
                            if (!ss.isNullable()) {
                                // the terminal is not an empty string
                                // no point of exploring this path
                                continue;
                            }
                        } else if (st instanceof NonTerminalState) {
                            NonTerminalState nts = (NonTerminalState) st;
                            if (!nullable.contains(nts.child)) {
                                // add back the NonTerminalState that we can't decide yet if it is nullable
                                keep.add(nts);
                                continue;
                            }
                        }
                        changed = true;
                        // either StartState, NonTerminalState, PrimitiveState
                        NextableState ns = (NextableState) st;
                        for (State ss : ns.next) {
                            if (!visited.contains(ss)) {
                                keep.add(ss);
                                visited.add(ss);
                            }
                        }
                        ee.setValue(keep);
                    }
                }
            }
        } while (changed);

        return nullable;
    }

    /**
     * Recursive DFS that traverses all the states and returns a set of all reachable {@Link NonTerminal}. 
     * @param start The state from which to run the collector.
     * @param visited Start with an empty Set<State>. Used as intermediate data.
     * @return A set of all reachable {@Link NonTerminal}.
     */
    private void getReachableNT(State start, Set<State> visited) {
        if (visited.contains(start))
            return;
        visited.add(start);
        if (start instanceof NextableState) {
            NextableState ns = (NextableState) start;
            for (State st : ns.next) {
                if (st instanceof NonTerminalState) {
                    all.add(((NonTerminalState) st).child);
                    getReachableNT(((NonTerminalState) st).child.entryState, visited);
                }
                getReachableNT(st, visited);
            }
        }
    }
}
