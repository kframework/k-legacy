// Copyright (c) 2014 K Team. All Rights Reserved.
package org.kframework.backend.symbolic;

import org.apache.commons.lang3.tuple.Pair;
import org.kframework.backend.Backend;
import org.kframework.backend.maude.MaudeKRunOptions;
import org.kframework.backend.maude.krun.MaudeExecutor;
import org.kframework.backend.maude.krun.MaudeModelChecker;
import org.kframework.krun.tools.Executor;
import org.kframework.krun.tools.LtlModelChecker;
import org.kframework.main.AbstractKModule;

import java.util.Collections;
import java.util.List;

/**
 * Created by andrei on 10/16/14.
 */
public class SymbolicBackendKModule extends AbstractKModule {

    @Override
    public List<Pair<String, Class<? extends Backend>>> backends() {
        return Collections.singletonList(Pair.<String, Class<? extends Backend>>of("symbolic", SymbolicKompileBackend.class));
    }

    @Override
    public List<Pair<String, Class<? extends Executor>>> executors() {
        return Collections.singletonList(Pair.<String, Class<? extends Executor>>of("symbolic", MaudeExecutor.class));
    }

    @Override
    public List<Pair<String, Class<? extends LtlModelChecker>>> ltlModelCheckers() {
        return Collections.singletonList(Pair.<String, Class<? extends LtlModelChecker>>of("symbolic", MaudeModelChecker.class));
    }
}
