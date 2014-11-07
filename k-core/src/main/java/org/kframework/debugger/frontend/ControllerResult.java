package org.kframework.debugger.frontend;

import org.kframework.kil.Attributes;
import org.kframework.krun.api.KRunGraph;
import org.kframework.krun.api.KRunState;
import org.kframework.transformation.Transformation;

/**
 *
 */
public class ControllerResult {
    Transformation transformation;                  //will be set depending on the kind of transformation

    private KRunState originalState;
    private KRunState newState;
    private DebuggerCommand command;                //represents the command issued by the frontend
    private Attributes a;
    

    public ControllerResult(Attributes a){
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
}
