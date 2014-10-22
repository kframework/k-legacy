// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.krun.ioserver.commands;

import org.kframework.backend.maude.MaudeFilter;
import org.kframework.kil.Sort;
import org.kframework.kil.Term;
import org.kframework.kil.loader.Context;
import org.kframework.krun.RunProcess;
import org.kframework.krun.api.io.FileSystem;
import org.kframework.utils.errorsystem.ParseFailedException;

import java.net.Socket;
import java.util.logging.Logger;

public class CommandParse extends Command {

    private String stringToParse;
    private Sort sort;
    private final Context context;
    private final RunProcess rp;

    public CommandParse(String[] args, Socket socket, Logger logger, Context context, FileSystem fs, RunProcess rp) {
        super(args, socket, logger, fs);
        this.context = context;
        this.rp = rp;

        sort = Sort.of(args[1]);
        stringToParse = args[2];
    }

    public void run() {
        try {
            Term kast = rp.runParser(context.krunOptions.configurationCreation.parser(context), stringToParse, true, sort, context);
            MaudeFilter mf = new MaudeFilter(context, rp.getKem());
            mf.visitNode(kast);
            succeed(mf.getResult().toString());
        } catch (ParseFailedException e) {
            fail("noparse");
        }
    }
}
