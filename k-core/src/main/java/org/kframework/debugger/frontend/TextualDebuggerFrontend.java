package org.kframework.debugger.frontend;

/**
 * Textual Debugger class that implements the Debugger Frontend Interface.
 * Later enhancements will allow the class to be used in a service loader.
 */
public class TextualDebuggerFrontend implements DebuggerFrontend{

    @Override
    public DebuggerStatus initFrontend() {
        return null;
    }

    @Override
    public DebuggerCommand getCommand() {
        return null;
    }

    @Override
    public DebuggerStatus publishResult() {
        return null;
    }
}
