// Copyright (c) 2014-2015 K Team. All Rights Reserved.
package org.kframework.kore.strategies;

import com.beust.jcommander.JCommander;
import com.beust.jcommander.ParameterException;
import com.google.inject.Inject;
import jline.ArgumentCompletor;
import jline.Completor;
import jline.ConsoleReader;
import jline.FileNameCompletor;
import jline.MultiCompletor;
import jline.SimpleCompletor;
import org.kframework.Rewriter;
import org.kframework.Strategy;
import org.kframework.attributes.Location;
import org.kframework.attributes.Source;
import org.kframework.definition.Rule;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kore.K;
import org.kframework.kore.KVariable;
import org.kframework.krun.KRun;
import org.kframework.krun.KRunDebuggerOptions;
import org.kframework.krun.KRunDebuggerOptions.CommandMatch;
import org.kframework.krun.KRunDebuggerOptions.CommandStep;
import org.kframework.krun.KRunOptions;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * Created by dwightguth on 5/8/15.
 */
public class Debug implements Strategy<Void> {

    private final KRunOptions krunOptions;
    private final CompiledDefinition compiledDef;
    private final KExceptionManager kem;

    @Inject
    public Debug(KRunOptions krunOptions, CompiledDefinition compiledDef, KExceptionManager kem) {
        this.krunOptions = krunOptions;
        this.compiledDef = compiledDef;
        this.kem = kem;
    }

    private static Object command(JCommander jc) {
        return jc.getCommands().get(jc.getParsedCommand()).getObjects().get(0);
    }

    @Override
    public Void execute(K k, Rewriter rewriter) {
        ConsoleReader reader;
        try {
            reader = new ConsoleReader();
        } catch (IOException e) {
            throw KEMException.internalError("IO error detected interacting with console", e);
        }
        // adding autocompletion and history feature to the stepper internal
        // commandline by using the JLine library
        reader.setBellEnabled(false);

        List<Completor> argCompletor = new LinkedList<Completor>();
        argCompletor.add(new SimpleCompletor(new String[] { "help",
                "exit", "resume", "step", "search", "select",
                "show-graph", "show-state", "show-transition", "save", "load", "read" }));
        argCompletor.add(new FileNameCompletor());
        List<Completor> completors = new LinkedList<Completor>();
        completors.add(new ArgumentCompletor(argCompletor));
        reader.addCompletor(new MultiCompletor(completors));

        k = rewriter.convert(k);

        while (true) {
            System.out.println();
            String input;
            try {
                input = reader.readLine("Command > ");
            } catch (IOException e) {
                throw KEMException.internalError("IO error detected interacting with console", e);
            }
            if (input == null) {
                // probably pressed ^D
                System.out.println();
                return null;
            }
            if (input.equals("")) {
                continue;
            }

            KRunDebuggerOptions options = new KRunDebuggerOptions();
            JCommander jc = new JCommander(options);
            jc.addCommand(options.help);
            jc.addCommand(options.resume);
            jc.addCommand(options.exit);
            jc.addCommand(options.match);
            jc.addCommand(options.showState);
            jc.addCommand(options.step);

            try {
                try {
                    jc.parse(input.split("\\s+"));

                    if (jc.getParsedCommand().equals("help")) {
                        if (options.help.command == null || options.help.command.size() == 0) {
                            jc.usage();
                        } else {
                            for (String command : options.help.command) {
                                if (jc.getCommands().containsKey(command)) {
                                    jc.usage(command);
                                }
                            }
                        }
                    } else if (command(jc) instanceof KRunDebuggerOptions.CommandExit) {
                        return null;
                    } else if (command(jc) instanceof KRunDebuggerOptions.CommandShowState) {
                        KRun.prettyPrint(compiledDef, krunOptions.output, System.out::print, k);
                    } else if (command(jc) instanceof KRunDebuggerOptions.CommandResume) {
                        k = new Execute(krunOptions).execute(k, rewriter);
                        print(k);
                    } else if (command(jc) instanceof CommandMatch) {
                        CommandMatch match = (CommandMatch) command(jc);
                        rewriter.match(k, true, findRule(match.file, match.line, rewriter));
                        // TODO(dwightguth): print result
                    } else if (command(jc) instanceof CommandStep) {
                        for (Rule r : rewriter.rules()) {
                            List<? extends Map<? extends KVariable,? extends K>> res = rewriter.match(k, false, r);
                            if (res.isEmpty()) continue;
                            System.out.print("rule ");
                            print(r.body());
                            System.out.print("requires ");
                            print(r.requires());
                            System.out.print("ensures ");
                            print(r.ensures());
                            k = rewriter.substitute(res.get(0), r);
                            break;
                        }
                        KRun.prettyPrint(compiledDef, krunOptions.output, System.out::print, k);
                    } else {
                        throw KEMException.criticalError("Unsupported krun debugger command " + jc.getParsedCommand());
                    }
                } catch (ParameterException e) {
                    throw KEMException.criticalError(e.getMessage(), e);
                }
            } catch (KEMException e) {
                kem.addKException(e.exception);
            } finally {
                kem.print();
                kem.getExceptions().clear();
            }
        }
    }

    public void print(K k) {
        KRun.prettyPrint(compiledDef, krunOptions.output, System.out::print, k);
    }

    private Rule findRule(String file, int line, Rewriter rewriter) {
        List<Rule> matchedRules = new ArrayList<>();
        for (Rule rule : rewriter.rules()) {
            if (rule.att().get(Source.class).isEmpty() || rule.att().get(Location.class).isEmpty()) {
                continue;
            }
            Source source = rule.att().get(Source.class).get();
            Location loc = rule.att().get(Location.class).get();
            if (source.source().toString().contains(file)
                    && loc.startLine() == line) {
                matchedRules.add(rule);
            }
        }
        if (matchedRules.size() == 0) {
            throw KEMException.criticalError("Could not find rule for auditing at line "
                    + line + " of file matching " + file);
        } else if (matchedRules.size() > 1) {
            System.err.println("Found multiple matches for rule to audit. Please select one:");
            for (int i = 0; i < matchedRules.size(); i++) {
                System.err.println(i + ": " + matchedRules.get(i));
            }
            do {
                System.err.print("> ");
                System.err.flush();
                try {
                    BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
                    int i = Integer.parseInt(br.readLine());
                    if (i >= 0 && i < matchedRules.size()) {
                        return matchedRules.get(i);
                    }
                } catch (NumberFormatException e) {
                } catch (IOException e) {
                    throw KEMException.criticalError("Could not read selection from stdin");
                }
            } while (true);
        } else {
            return matchedRules.get(0);
        }
    }
}
