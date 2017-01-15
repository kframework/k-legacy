package org.kframework.backend.java.symbolic;

import org.kframework.backend.java.kil.GlobalContext;
import org.kframework.minikore.MiniKore;
import org.kframework.rewriter.Rewriter;
import org.kframework.utils.errorsystem.KExceptionManager;

import java.util.function.Function;

/**
 * Uses MiniKore Definition to initialize the rewriter.
 */
public class InitializeMiniKoreRewriter implements Function<MiniKore.Module, Rewriter> {

    @Override
    public Rewriter apply(MiniKore.Module module) {
        //TODO: Generate the rewriter based on MiniKore Module, instead of Kore Module
        return null;
    }

    public static class InitializeMiniKoreDefinition {

        public MiniKore.Definition invoke(MiniKore.Module module, KExceptionManager kem, GlobalContext global) {
            //TODO: The parallel class seems to do some sort of caching.
            return null;
        }

    }
}
