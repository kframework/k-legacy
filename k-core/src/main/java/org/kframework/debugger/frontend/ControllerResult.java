package org.kframework.debugger.frontend;

import org.kframework.kil.Attributes;
import org.kframework.krun.api.KRunGraph;
import org.kframework.krun.api.KRunState;
import org.kframework.krun.api.SearchResults;
import org.kframework.transformation.Transformation;

/**
 *  Format of the Result:
 *  Pass the required values in the constructor as parameters.
 *  For eg, for a step operation, valid call is
 *  ControllerResult(Attributes, originalState, SteppedState, originalCommand, null, Status.VALID)
 */
public class ControllerResult {
    enum Status{
        VALID, INVALID
    }
    Transformation transformation;                  //will be set depending on the kind of transformation
    private KRunState originalState;
    private KRunState newState;
    private DebuggerCommand command;                //represents the command issued by the frontend
    private Attributes a;
    private SearchResults searchResults;
    public SearchResults getSearchResults() {
        return searchResults;
    }

    public void setSearchResults(SearchResults searchResults) {
        this.searchResults = searchResults;
    }

    public ControllerResult(Attributes a, originalState, newState, command, searchResults, Status){
        this.a = a;
    }

    public void setTransformation(Transformation transformation) {
        this.transformation = transformation;
    }

    public void setOriginalState(KRunState originalState){
        this.originalState = originalState;
    }
    public void setNewState(KRunState newState){
        this.newState = newState;
    }

    public void setCommand(DebuggerCommand command){
        this.command = command;
    }

    public Transformation getTransformation() {
        return transformation;
    }

    public KRunState getOriginalState() {
        return originalState;
    }

    public KRunState getNewState() {
        return newState;
    }

    public DebuggerCommand getCommand() {
        return command;
    }

    public Attributes getAttributes(){
        return a;
    }
}
