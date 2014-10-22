// Copyright (c) 2012-2014 K Team. All Rights Reserved.
package org.kframework.krun.ioserver.main;

import org.kframework.kil.loader.Context;
import org.kframework.krun.RunProcess;
import org.kframework.krun.api.io.FileSystem;
import org.kframework.krun.ioserver.filesystem.portable.PortableFileSystem;

import java.util.logging.Logger;

public class MainServer implements Runnable {
    public int _port;
    public boolean _started;
    private Logger _logger;
    private final Context context;
    private final FileSystem fs;
    private final RunProcess rp;

    public MainServer(int port, Logger logger, Context context, RunProcess rp) {
        this.context = context;
        _port = port;
        _logger = logger;
        fs = new PortableFileSystem();
        this.rp = rp;
    }
    public void run() {
        IOServer server = new IOServer(_port, _logger, context, fs, rp);
        _port = server.port; // in case server changes port
        _started = true;
        try {
            server.acceptConnections();
        } catch (java.io.IOException e) {
            _logger.severe("Error accepting connection:" + e);
        }
    }
}
