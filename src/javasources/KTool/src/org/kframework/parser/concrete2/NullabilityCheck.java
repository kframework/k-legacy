package org.kframework.parser.concrete2;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Queue;
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
        Helper hp = new Helper();
        return hp.checkNullability2(startNt);
    }

    /**
     * Helper class that holds temporary information for the nullability check.
     * @author Radu
     *
     */
    private static class Helper {
        Set<NonTerminal> nullable = new HashSet<>();
        Map<NonTerminal, Queue<State>> dependencies = new HashMap<>();
        Set<State> visited = new HashSet<>();

        public Set<NonTerminal> checkNullability2(NonTerminal startNt) {
            Set<NonTerminal> all = getReachableNT(startNt.entryState);
            all.add(startNt);
            visited.clear();

            // insert all the non-terminals and their start state
            for (NonTerminal nt : all)
                dependencies.put(nt, new LinkedList<State>(Arrays.asList(nt.entryState)));

            boolean changed = false;
            do {
                changed = false;
                for (Entry<NonTerminal, Queue<State>> ee : dependencies.entrySet()) {
                    // for each non terminal set
                    State st = null;
                    while ((st = ee.getValue().poll()) != null) {
                        if (st instanceof ExitState) {
                            nullable.add(ee.getKey());
                            dependencies.remove(ee.getKey());
                            changed = true;
                            break;
                        } else { // nextable states
                            assert st instanceof NextableState : "Expected nextable states here!";
                            if (st instanceof PrimitiveState) {
                                PrimitiveState ss = (PrimitiveState) st;
                                Set<MatchResult> matchResults = ss.matches("", 0, null);
                                if (matchResults.size() == 0) {
                                    // the terminal is not an empty string
                                    // no point of exploring this path
                                    break;
                                }
                            } else if (st instanceof NonTerminalState) {
                                NonTerminalState nts = (NonTerminalState) st;
                                if (!nullable.contains(nts.child)) {
                                    // add back the NonTerminalState that we can't decide yet if it is nullable
                                    ee.getValue().add(nts);
                                    break;
                                }
                            }
                            changed = true;
                            // either StartState, NonTerminalState, PrimitiveState
                            // so go to the next states
                            NextableState ns = (NextableState) st;
                            for (State ss : ns.next) {
                                ee.getValue().add(ss);
                            }
                        }
                    }
                    if (changed) // possible to have modified the dependencies object so start over
                        break;
                }
            } while (changed);

            return nullable;
        }

        /**
         * Recursive DFS that traverses all the states and returns a set of all reachable {@Link NonTerminal}. 
         * @param start The state from which to run the collector.
         * @return A set of all reachable {@Link NonTerminal}.
         */
        private Set<NonTerminal> getReachableNT(State start) {
            if (visited.contains(start))
                return new HashSet<>();
            visited.add(start);
            if (start instanceof NextableState) {
                NextableState ns = (NextableState) start;
                Set<NonTerminal> ret = new HashSet<>();
                for (State st : ns.next) {
                    if (st instanceof NonTerminalState) {
                        ret.add(((NonTerminalState) st).child);
                        ret.addAll(getReachableNT(((NonTerminalState) st).child.entryState));
                    }
                    ret.addAll(getReachableNT(st));
                }
                return ret;
            }
            return new HashSet<>();
        }
    }
}
