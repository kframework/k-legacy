// Copyright (c) 2013-2016 K Team. All Rights Reserved.
package org.kframework.kompile;

import org.kframework.definition.Definition;
import org.kframework.main.FrontEnd;
import org.kframework.minikore.KoreToMini;
import org.kframework.minikore.KoreToMiniToKore;
import org.kframework.minikore.MiniToText;
import org.kframework.minikore.MiniToTextToMini;
import org.kframework.minikore.TextToMini;
import org.kframework.parser.UserParser;
import org.kframework.utils.BinaryLoader;
import org.kframework.utils.Stopwatch;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KException;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.file.JarInfo;
import org.kframework.utils.inject.DefinitionLoadingModule;

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
        //CompiledDefinition def = kompile.run(options.outerParsing.mainDefinitionFile(files), options.mainModule(files), options.syntaxModule(files), koreBackend.steps());
        Definition parsedDef = kompile.parseDefinition(options.outerParsing.mainDefinitionFile(files), options.mainModule(files), options.syntaxModule(files));
        CompiledDefinition compiledDef = kompile.compile(parsedDef, koreBackend.steps());
        ParsedDefinitionWrapper wrapper = new ParsedDefinitionWrapper(options, parsedDef);
        saveModuleDerivedParser(wrapper, wrapper.mainSyntaxModuleName(), kem);
        save(compiledDef);
        koreBackend.accept(compiledDef);
        loader.saveOrDie(files.resolveKompiled(FileUtil.TIMESTAMP), "");
        sw.printIntermediate("Save to disk");
        sw.printTotal("Total");
        return 0;
    }

    public void saveModuleDerivedParser(ParsedDefinitionWrapper wrapper, String moduleName, KExceptionManager kem) {
        UserParser parser = wrapper.getModuleDerviedParser(moduleName, kem);
        String fileName = "extras/" + moduleName +"_Parser.bin";
        loader.saveOrDie(files.resolveKompiled(fileName), parser);
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

    // for serialization/deserialization test
    public void saveTest(CompiledDefinition def) {
        KoreToMiniToKore.apply(def.kompiledDefinition);
        KoreToMiniToKore.apply(def.getParsedDefinition());
        MiniToTextToMini.assertequal(KoreToMini.apply(def.kompiledDefinition), DefinitionLoadingModule.parseKore(files));
    }
}

