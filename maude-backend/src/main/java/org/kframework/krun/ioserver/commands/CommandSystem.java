// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.krun.ioserver.commands;

import org.kframework.kil.loader.Context;
import org.kframework.krun.RunProcess;
import org.kframework.krun.RunProcess.ProcessOutput;
import org.kframework.krun.api.io.FileSystem;

import java.net.Socket;
import java.util.logging.Logger;
import java.util.Map;
import java.util.HashMap;

public class CommandSystem extends Command {

    private String[] cmd;
    private final RunProcess rp;
    private final Context context;

    public CommandSystem(String[] args, Socket socket, Logger logger, FileSystem fs, RunProcess rp, Context context) {
        super(args, socket, logger, fs);
        this.rp = rp;
        this.context = context;

        int length = args.length - 1 - 3 /* garbage */;
        cmd = new String[length];
        System.arraycopy(args, 1, cmd, 0, length);
    }

    public void run() {
      //for (String c : cmd) { System.out.println(c);
        Map<String, String> environment = new HashMap<>();
        ProcessOutput output = rp.execute(context.files.resolveWorkingDirectory("."), environment, cmd);

        String stdout = output.stdout != null ? output.stdout : "";
        String stderr = output.stderr != null ? output.stderr : "";
        succeed(Integer.toString(output.exitCode), stdout.trim(), stderr.trim(), "#EOL");
    }
}
