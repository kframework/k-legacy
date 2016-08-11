// Copyright (c) 2013-2016 K Team. All Rights Reserved.
package org.kframework.kompile;

import org.kframework.main.FrontEnd;
import org.kframework.utils.BinaryLoader;
import org.kframework.utils.Stopwatch;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.file.JarInfo;

public class KompileFrontEnd extends FrontEnd {



    private final KompileOptions options;
    private final org.kframework.kore.compile.Backend koreBackend;
    private final Stopwatch sw;
    private final KExceptionManager kem;
    private final BinaryLoader loader;
    private final FileUtil files;

    public KompileFrontEnd(
            KompileOptions options,
            org.kframework.kore.compile.Backend koreBackend,
            Stopwatch sw,
            KExceptionManager kem,
            BinaryLoader loader,
            FileUtil files) {
        super(kem, options.global, files);
        this.options = options;
        this.koreBackend = koreBackend;
        this.sw = sw;
        this.kem = kem;
        this.loader = loader;
        this.files = files;
    }

    @Override
    public int run() {
        if (!options.outerParsing.mainDefinitionFile(files).exists()) {
            throw KEMException.criticalError("Definition file doesn't exist: " +
                    options.outerParsing.mainDefinitionFile(files).getAbsolutePath());
        }

        Kompile kompile = new Kompile(options, files, kem, sw);
        CompiledDefinition def = kompile.run(options.outerParsing.mainDefinitionFile(files), options.mainModule(files), options.syntaxModule(files), koreBackend.steps(kompile));
        loader.saveOrDie(files.resolveKompiled("compiled.bin"), def);
        koreBackend.accept(def);
        loader.saveOrDie(files.resolveKompiled("timestamp"), "");
        sw.printIntermediate("Save to disk");
        sw.printTotal("Total");
        return 0;
    }
}

