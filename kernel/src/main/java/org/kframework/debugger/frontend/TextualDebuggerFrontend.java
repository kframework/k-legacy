// Copyright (c) 2013-2014 K Team. All Rights Reserved.

package org.kframework.debugger.frontend;

import com.beust.jcommander.JCommander;
import jline.*;
import org.kframework.krun.KRunDebuggerOptions;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

/**
 * Textual Debugger class that implements the Debugger Frontend Interface.
 * Later enhancements will allow the class to be used in a service loader.
 */
public class TextualDebuggerFrontend implements DebuggerFrontend{

    private ConsoleReader console;
    KRunDebuggerOptions options;
    JCommander jc;
    /*
     * K uses Guice. This constructor will be used to create the
     * instance and then injected into the controller
     */
    public TextualDebuggerFrontend(){

    }

    @Override
    public FrontendStatus initFrontend() {
        try {
            console = new ConsoleReader();
            console.setDefaultPrompt("Kdb > ");
            List<Completor> argCompletor = new LinkedList<Completor>();
            argCompletor.add(new SimpleCompletor(new String[] { "help",
                    "exit", "resume", "step", "search", "select",
                    "show-graph", "show-state", "show-transition", "save", "load", "read" }));
            argCompletor.add(new FileNameCompletor());
            List<Completor> completors = new LinkedList<Completor>();
            completors.add(new ArgumentCompletor(argCompletor));
            console.addCompletor(new MultiCompletor(completors));
        }
        catch(IOException e){
            return FrontendStatus.FAILURE;
        }
        options = new KRunDebuggerOptions();
        jc = new JCommander(options);
        jc.addCommand(options.help);
        jc.addCommand(options.step);
        jc.addCommand(options.search);
        jc.addCommand(options.select);
        jc.addCommand(options.showGraph);
        jc.addCommand(options.showState);
        jc.addCommand(options.showTransition);
        jc.addCommand(options.resume);
        jc.addCommand(options.exit);
        jc.addCommand(options.save);
        jc.addCommand(options.load);
        jc.addCommand(options.read);
        return FrontendStatus.SUCCESS;
    }

    @Override
    public DebuggerCommand getCommand() {
        boolean invalidFlag = false;
        DebuggerCommand command = new DebuggerCommand();
        String input;
        while(!invalidFlag) {
            //ask the user for input
            System.out.println();
            try {
                input = console.readLine("Kdb > ");
                if(input == null){
                    command.setCommand(GenericCommand.EXIT);
                    return command;
                }
                if(input.equals("")){
                    continue;
                }
                jc.parse(input.split("\\s+"));
                //Help command can be customized, in this case print to console
                if (jc.getParsedCommand().equals("help")) {
                    if (options.help.command == null || options.help.command.size() == 0) {
                        jc.usage();
                    } else {
                        for (String cmd : options.help.command) {
                            if (jc.getCommands().containsKey(cmd)) {
                                jc.usage(cmd);
                            }
                        }
                    }
                }  else if (command(jc) instanceof KRunDebuggerOptions.CommandStep) {
                    //intended usage of the command object
                    command.setCommand(GenericCommand.STEP);
                    command.setStepCount(options.step.numSteps);
                    return command;

                } else if (command(jc) instanceof KRunDebuggerOptions.CommandSearch) {
                    command.setCommand(GenericCommand.SEARCH);
                    System.out.println(options.search.numSteps);
                    command.setStepCount(options.search.numSteps);
                    return command;

                } else if(command(jc) instanceof KRunDebuggerOptions.CommandSelect) {
                    command.setCommand(GenericCommand.SELECT);
                    command.setStateOne(options.select.stateId);
                    return command;
                } else if(command(jc) instanceof KRunDebuggerOptions.CommandResume) {
                    command.setCommand(GenericCommand.RESUME);
                    return command;
                } else if(command(jc) instanceof KRunDebuggerOptions.CommandShowState) {
                    command.setCommand(GenericCommand.GET_STATE);
                    command.setStateOne(options.showState.stateId);
                    return command;
                } else if(command(jc) instanceof KRunDebuggerOptions.CommandShowTransition) {
                    command.setCommand(GenericCommand.GET_TRANSITION);
                    command.setStateOne(options.showTransition.state1());
                    command.setStateTwo(options.showTransition.state2());
                    return command;
                }


            }
            catch(IOException e){
                command.setCommand(GenericCommand.EXIT);
            }

        }
        return null;
    }

    private static Object command(JCommander jc) {
        return jc.getCommands().get(jc.getParsedCommand()).getObjects().get(0);
    }

    @Override
    public FrontendStatus publishResult(ControllerResult result) {
        switch(result.getCommand().getGenericCommand()){
            case STEP:
                System.out.println(result.getTransformation().run(result.getNewState(), result.getAttributes()));
                return FrontendStatus.SUCCESS;
            case SEARCH:
                System.out.println(result.getTransformation().run(result.getSearchResults(), result.getAttributes()));
                return FrontendStatus.SUCCESS;
            case SELECT:
                System.out.println("Selected State : ");
                System.out.println(result.getTransformation().run(result.getNewState(), result.getAttributes()));
                return FrontendStatus.SUCCESS;
            case RESUME:
                System.out.println(result.getTransformation().run(result.getNewState(), result.getAttributes()));
            case GET_STATE:
                System.out.println(result.getTransformation().run(result.getNewState(), result.getAttributes()));
            case GET_TRANSITION:
                System.out.println(result.getTransformation().run(result.getTransition(), result.getAttributes()));

        }
        return FrontendStatus.FAILURE;
    }
}
