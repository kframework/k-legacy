package org.kframework.debugger;

import org.kframework.utils.Goal;

import java.util.List;

public class ProofState {
    private List<Goal> goalList;
    private int activeId;

    public ProofState(List<Goal> goalList, int activeId) {
        this.goalList = goalList;
        this.activeId = activeId;
    }

    public List<Goal> getGoalList() {
        return goalList;
    }

    public int getActiveId() {
        return activeId;
    }
}
