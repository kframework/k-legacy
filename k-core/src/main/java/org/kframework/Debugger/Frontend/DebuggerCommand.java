package org.kframework.debugger.frontend;

/**
 *  Represents a command that will be given to the controller.
 *
 */
public class DebuggerCommand {
    private int stepCount;
    private Command command;
    private int stateOne;
    private int stateTwo;

    private int INVALID = -1;

    public DebuggerCommand(){
        stepCount = INVALID;
        command = Command.INVALID;
        stateOne = INVALID;
        stateTwo = INVALID;
    }

    public void setStepCount(int stepCount){
        this.stepCount = stepCount;
    }

    public void setCommand(Command command){
        this.command = command;
    }

    public void setStateOne(int stateOne){
        this.stateOne = stateOne;
    }

    public void setStateTwo(int stateTwo){
        this.stateTwo = stateTwo;
    }
}
