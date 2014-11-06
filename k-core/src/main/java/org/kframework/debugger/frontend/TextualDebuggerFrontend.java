package org.kframework.debugger.frontend;

import jline.*;
import org.kframework.debugger.DebuggerStatus;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

/**
 * Textual Debugger class that implements the Debugger Frontend Interface.
 * Later enhancements will allow the class to be used in a service loader.
 */
public class TextualDebuggerFrontend implements DebuggerFrontend{

    private ConsoleReader reader;

    /*
     * K uses Guice. This constructor will be used to create the
     * instance and then injected into the controller
     */
    public TextualDebuggerFrontend(){

    }

    @Override
    public FrontendStatus initFrontend() {
        try {
            reader = new ConsoleReader();
            reader.setDefaultPrompt("Kdb > ");
            List<Completor> argCompletor = new LinkedList<Completor>();
            argCompletor.add(new SimpleCompletor(new String[] { "help",
                    "exit", "resume", "step", "search", "select",
                    "show-graph", "show-state", "show-transition", "save", "load", "read" }));
            argCompletor.add(new FileNameCompletor());
            List<Completor> completors = new LinkedList<Completor>();
            completors.add(new ArgumentCompletor(argCompletor));
            reader.addCompletor(new MultiCompletor(completors));
        }
        catch(IOException e){
            return FrontendStatus.FAILURE;
        }
        return FrontendStatus.SUCCESS;
    }

    @Override
    public DebuggerStatus getCommand() {
        boolean invalidFlag = false;
        while(!invalidFlag) {
            
        }
    }

    @Override
    public FrontendStatus publishResult() {
        return null;
    }
}
