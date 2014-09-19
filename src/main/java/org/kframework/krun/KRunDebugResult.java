package org.kframework.krun;


import org.kframework.backend.java.kil.Rule;
import org.kframework.krun.api.KRunState;
import java.util.List;

public class KRunDebugResult {
    private List<KRunState> stateList;
    private List<Rule> ruleList;

    public KRunDebugResult(List<KRunState> stateList, List<Rule> ruleList){
        this.stateList = stateList;
        this.ruleList = ruleList;
    }
    public List<KRunState> getStateList(){
        return stateList;
    }
    public List<Rule> getRuleList(){
        return ruleList;
    }
}
