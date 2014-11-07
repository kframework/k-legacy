package org.kframework.debugger.frontend;

/**
 *  Represents a command that will be given to the controller.
 *
 */
public class DebuggerCommand {
    private int stepCount;
    private GenericCommand command;
    private int stateOne;
    private int stateTwo;

    private int INVALID = -1;

    public DebuggerCommand(){
        stepCount = INVALID;
        command = GenericCommand.INVALID;
        stateOne = INVALID;
        stateTwo = INVALID;
    }

    public void setStepCount(int stepCount){
        this.stepCount = stepCount;
    }

    public void setCommand(GenericCommand command){
        this.command = command;
    }

    public void setStateOne(int stateOne){
        this.stateOne = stateOne;
    }

    public void setStateTwo(int stateTwo){
        this.stateTwo = stateTwo;
    }

    public GenericCommand getCommand(){
        return command;
    }

    public int getStepCount(){
        return stepCount;
    }
}
