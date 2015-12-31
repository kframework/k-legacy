// Copyright (c) 2014-2015 K Team. All Rights Reserved.
package org.kframework.backend;

import org.kframework.utils.Stopwatch;

public abstract class PosterBackend {

    protected final Stopwatch sw;

    public PosterBackend(Stopwatch sw) {
        this.sw = sw;
    }

    public abstract void run();

}
