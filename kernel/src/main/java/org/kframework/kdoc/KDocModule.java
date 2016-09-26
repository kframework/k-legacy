// Copyright (c) 2014-2016 K Team. All Rights Reserved.
package org.kframework.kdoc;

import org.kframework.backend.PosterBackend;
import org.kframework.backend.latex.LatexBackend;
import org.kframework.main.FrontEnd;
import org.kframework.main.GlobalOptions;
import org.kframework.main.Tool;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.inject.OuterParsingModule;
import org.kframework.utils.options.OuterParsingOptions;

import java.util.Map;

public class KDocModule {

    // TODO(Daejun): remove this module

    GlobalOptions globalOptions(KDocOptions options) {
        return options.global;
    }

    OuterParsingOptions outerParsingOptions(KDocOptions options) {
        return options.outerParsing;
    }

    PosterBackend getBackend(KDocOptions options, Map<String, PosterBackend> map, KExceptionManager kem) {
        PosterBackend backend = map.get(options.format);
        if (backend == null) {
            throw KEMException.criticalError("Invalid poster format: " + options.format
                    + ". It should be one of " + map.keySet());
        }
        return backend;
    }

}
