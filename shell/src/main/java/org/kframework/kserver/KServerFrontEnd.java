// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.kserver;
import java.io.File;
import java.io.PrintStream;
import java.net.InetAddress;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.fusesource.jansi.AnsiConsole;
import org.fusesource.jansi.AnsiOutputStream;
import org.kframework.main.FrontEnd;
import org.kframework.main.Main;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.file.JarInfo;
import org.kframework.utils.file.TTYInfo;
import org.kframework.utils.inject.CommonModule;
import org.kframework.utils.inject.JCommanderModule;

import com.google.common.collect.ImmutableList;
import com.martiansoftware.nailgun.NGContext;
import com.martiansoftware.nailgun.NGServer;
import com.martiansoftware.nailgun.ThreadLocalPrintStream;


public class KServerFrontEnd extends FrontEnd {

    public KServerFrontEnd(
            KExceptionManager kem,
            KServerOptions options,
            FileUtil files) {
        super(kem, options.global, files);
        this.options = options;
    }

    private static KServerFrontEnd instance;
    private static Thread threadInstance;
    private static final ImmutableList<String> tools = ImmutableList.of("-kompile", "-krun", "-kast", "-kdoc", "-ktest", "-kdep");

    private final KServerOptions options;

    @Override
    protected int run() {
        NGServer server = new NGServer(InetAddress.getLoopbackAddress(), options.port);
        Thread t = new Thread(server);
        instance = this;
        threadInstance = t;
        t.start();

        int runningPort = server.getPort();
        while (runningPort == 0) {
            try {
                Thread.sleep(50L);
            } catch (InterruptedException e) {}
            runningPort = server.getPort();
        }
        System.out.println("K server started on 127.0.0.1:" + options.port);

        try {
            t.join();
            return 0;
        } catch (InterruptedException e) {
            //application is about to die
            return 0;
        }
    }

    public static KServerFrontEnd instance() {
        return instance;
    }

    public int run(String tool, String[] args, File workingDir, Map<String, String> env) {
        ThreadLocalPrintStream system_out = (ThreadLocalPrintStream) System.out;
        ThreadLocalPrintStream system_err = (ThreadLocalPrintStream) System.err;

        TTYInfo tty = CommonModule.ttyInfo(env);
        if (tty.stdout) {
            system_out.init(new PrintStream(AnsiConsole.wrapOutputStream(system_out.getPrintStream())));
        } else {
            system_out.init(new PrintStream(new AnsiOutputStream(system_out.getPrintStream())));
        }
        if (tty.stderr) {
            system_err.init(new PrintStream(AnsiConsole.wrapOutputStream(system_err.getPrintStream())));
        } else {
            system_err.init(new PrintStream(new AnsiOutputStream(system_err.getPrintStream())));
        }

        int result = Main.runApplication(tool, args, workingDir, env);
        System.out.flush();
        System.err.flush();
        return result;
    }

    public static void nailMain(NGContext context) {
        System.setSecurityManager(null);
        context.getNGServer().shutdown(true);
    }
}
