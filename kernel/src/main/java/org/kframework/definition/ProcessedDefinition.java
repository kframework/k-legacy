package org.kframework.definition;

import org.kframework.kompile.KompileOptions;
import org.kframework.minikore.implementation.MiniKore;

/** Definition Contains the minikore Definition + Extras That are needed for the Backend to Function.
 * The Rewriter is a part of the backend. It needs two things to function - A Kore start term, and a Kore Definition (axioms for reachability logic/dynamic matching logic.
 * The Definition may contain other utilities needed by the backend, such as a parser/pretty printer, and hooks, that may be needed for the rewriter to function correctly.
 */

public class ProcessedDefinition {
    public KompileOptions kompileOptions;
    public MiniKore.Definition definition;
    public MiniKore.Module mainModule;


    public ProcessedDefinition(KompileOptions kompileOptions, MiniKore.Definition definition) {
        this.kompileOptions = kompileOptions;
        this.definition = definition;
        this.mainModule = null;
    }

    public MiniKore.Module getMainModule() {
        if (mainModule != null) {
            return mainModule;
        }

        return mainModule;
    }

}
