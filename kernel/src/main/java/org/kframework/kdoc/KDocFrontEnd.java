// Copyright (c) 2014-2016 K Team. All Rights Reserved.
package org.kframework.kdoc;

import org.kframework.backend.PosterBackend;
import org.kframework.main.FrontEnd;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.file.JarInfo;
import org.kframework.utils.inject.CommonModule;
import org.kframework.utils.inject.JCommanderModule;

import java.util.ArrayList;
import java.util.List;


public class KDocFrontEnd extends FrontEnd {

    private final KDocOptions options;
    private final PosterBackend backend;

    public KDocFrontEnd(
            KDocOptions options,
            KExceptionManager kem,
            GlobalOptions globalOptions,
            FileUtil files,
            PosterBackend backend) {
        super(kem, globalOptions, files);
        this.options = options;
        this.backend = backend;
    }

    @Override
    protected int run() {
        return 0;
    }

}
