// Copyright (c) 2013-2016 K Team. All Rights Reserved.
package org.kframework.kompile;

import org.kframework.definition.Definition;
import org.kframework.main.FrontEnd;
import org.kframework.minikore.converters.KoreToMini;
import org.kframework.minikore.converters.KoreToMiniToKore;
import org.kframework.kore.parser.KoreToText;
import org.kframework.kore.parser.MiniToTextToMini;
import org.kframework.parser.UserParser;
import org.kframework.utils.BinaryLoader;
import org.kframework.utils.Stopwatch;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.inject.DefinitionLoadingModule;

public class KompileFrontEnd extends FrontEnd {



    private final KompileOptions options;
    private final org.kframework.frontend.compile.Backend koreBackend;
    private final Stopwatch sw;
    private final KExceptionManager kem;
    private final BinaryLoader loader;
    private final FileUtil files;

    public KompileFrontEnd(
            KompileOptions options,
            org.kframework.frontend.compile.Backend koreBackend,
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
        //CompiledDefinition def = kompile.run(options.outerParsing.mainDefinitionFile(files), options.mainModule(files), options.syntaxModule(files), koreBackend.steps()); //deprecated
        Definition parsedDef = kompile.parseDefinition(options.outerParsing.mainDefinitionFile(files), options.mainModule(files), options.syntaxModule(files));
        ParserGenerator generator = new ParserGenerator(options, parsedDef);
        CompiledDefinition compiledDef = kompile.compile(parsedDef, koreBackend.steps());

        saveKompileMetaInfo(generator);
        saveParser(generator);
        save(compiledDef);
        koreBackend.accept(compiledDef);
        loader.saveOrDie(files.resolveKompiled(FileUtil.TIMESTAMP), "");
        sw.printIntermediate("Save to disk");
        sw.printTotal("Total");
        return 0;
    }

    public void saveKompileMetaInfo(ParserGenerator generator) {
        KompileMetaInfo info = new KompileMetaInfo(generator.mainSyntaxModuleName(), generator.configurationVariableDefaultSorts);
        files.saveToKompiled(FileUtil.KOMPILE_META_INFO_TXT, info.serialize());
    }

    public void saveParser(ParserGenerator generator) {
        String mainSyntaxModuleName = generator.mainSyntaxModuleName();
        //Save default parser (main syntax module)
        UserParser parser = generator.getParser(mainSyntaxModuleName, kem);
        String modulePath = FileUtil.moduleDerivedParserPath(mainSyntaxModuleName);
        loader.saveOrDie(files.resolveKompiled(modulePath), parser);
        // Save parser generator
        loader.saveOrDie(files.resolveKompiled(FileUtil.PARSER_GENERATOR_BIN), generator);
    }

    // NOTE: should be matched with org.kframework.utils.inject.DefinitionLoadingModule.koreDefinition()
    public void save(CompiledDefinition def) {
        org.kframework.kore.Definition koreDefinition = KoreToMini.apply(def.kompiledDefinition);
        files.saveToKompiled(FileUtil.KORE_TXT, KoreToText.apply(koreDefinition));
        SerializableKoreDefinition wrappedDefinition = new SerializableKoreDefinition(koreDefinition);
        files.saveToKompiledFST(FileUtil.KORE_BIN, wrappedDefinition.getClass(), wrappedDefinition);
//        loader.saveOrDie(files.resolveKompiled(FileUtil.KOMPILED_DEFINITION_BIN), def.kompiledDefinition); //Deprecated
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

