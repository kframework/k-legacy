package org.kframework.debugger.frontend;

/**
 *  The Interface for every frontend. The Instance is
 *  dependency injected into the Controller.
 */
public interface DebuggerFrontend {

    /**
     * Function called to begin the Debugger's FrontEnd. All Initialization tasks
     * such as setting prompt for Textual Debugger e.t.c., should be performed here.
     * @return DebuggerStatus Enum type specifying the status of the operation
     */
    public abstract DebuggerStatus initFrontend();

    /**
     * Called by the controller when it's ready to receive a command. The frontend should
     * take input from the user, and return a DebuggerCommand instance, specifying the details of the
     * command
     * @return DebuggerStatus instance specifying the details of the intended command.
     */
    public abstract DebuggerCommand getCommand();

    /**
     * Called by the controller when the results are ready to be published.
     * @return DebuggerStatus specifying the result of the operation. Specifying FAILURE will lead to the
     *         same result in function being called again with the same result
     */
    public abstract DebuggerStatus publishResult();

}
