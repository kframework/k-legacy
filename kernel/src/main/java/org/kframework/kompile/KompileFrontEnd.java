// Copyright (c) 2013-2016 K Team. All Rights Reserved.
package org.kframework.kompile;

import org.kframework.main.FrontEnd;
import org.kframework.minikore.KoreToMini;
import org.kframework.minikore.KoreToMiniToKore;
import org.kframework.minikore.MiniToText;
import org.kframework.minikore.MiniToTextToMini;
import org.kframework.minikore.TextToMini;
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
        CompiledDefinition def = kompile.run(options.outerParsing.mainDefinitionFile(files), options.mainModule(files), options.syntaxModule(files), koreBackend.steps());
        save(def);
        koreBackend.accept(def);
        loader.saveOrDie(files.resolveKompiled(FileUtil.TIMESTAMP), "");
        sw.printIntermediate("Save to disk");
        sw.printTotal("Total");
        return 0;
    }

    // NOTE: should be matched with org.kframework.utils.inject.DefinitionLoadingModule.koreDefinition()
    public void save(CompiledDefinition def) {
        files.saveToKompiled(FileUtil.KORE_TXT, MiniToText.apply(KoreToMini.apply(def.kompiledDefinition)));
        // loader.saveOrDie(files.resolveKompiled(FileUtil.KOMPILED_DEFINITION_BIN), def.kompiledDefinition); // deprecated
        loader.saveOrDie(files.resolveKompiled(FileUtil.KOMPILE_OPTIONS_BIN), def.kompileOptions);
        loader.saveOrDie(files.resolveKompiled(FileUtil.PARSED_DEFINITION_BIN), def.getParsedDefinition());
        loader.saveOrDie(files.resolveKompiled(FileUtil.TOP_CELL_INITIALIZER_BIN), def.topCellInitializer);
        // saveTest(def);
    }

    public void saveTest(CompiledDefinition def) {
        KoreToMiniToKore.apply(def.kompiledDefinition); // for serialization/deserialization test
        KoreToMiniToKore.apply(def.getParsedDefinition()); // for serialization/deserialization test
        MiniToTextToMini.assertequal(KoreToMini.apply(def.kompiledDefinition), new TextToMini().parse(files.resolveKompiled(FileUtil.KORE_TXT))); // for serialization/deserialization test
    }
}

