// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.utils;

import org.apache.commons.lang3.tuple.Pair;
import org.kframework.HookProvider;
import org.kframework.backend.java.symbolic.JavaBackend;
import org.kframework.backend.java.symbolic.JavaExecutionOptions;
import org.kframework.kore.KORE;
import org.kframework.kore.KToken;
import org.kframework.kore.Sort;
import org.kframework.krun.KRunOptions;
import org.kframework.rewriter.Rewriter;
import org.kframework.attributes.Source;
import org.kframework.backend.java.symbolic.InitializeRewriter;
import org.kframework.backend.java.symbolic.Stage;
import org.kframework.builtin.Sorts;
import org.kframework.definition.Module;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kompile.Kompile;
import org.kframework.kompile.KompileOptions;
import org.kframework.kore.K;
import org.kframework.krun.KRun;
import org.kframework.krun.api.io.FileSystem;
import org.kframework.krun.ioserver.filesystem.portable.PortableFileSystem;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.options.SMTOptions;

import java.io.File;
import java.io.IOException;
import java.lang.invoke.MethodHandle;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;

import static org.kframework.kore.KORE.KToken;

/**
 * Created by Manasvi on 6/19/15.
 * <p>
 * Create this object to use for Tests.
 * <p>
 * Contains utilities used across Tests.
 */

public class KoreUtils {

    public final CompiledDefinition compiledDef;
    public final KExceptionManager kem;
    public final BiFunction<String, Source, K> programParser;
    public InitializeRewriter initializeRewriter;
    public Rewriter rewriter;

    protected File testResource(String baseName) throws URISyntaxException {
        return new File(KoreUtils.class.getResource(baseName).toURI());
    }

    public KoreUtils(String fileName, String mainModuleName, String mainProgramsModuleName, KExceptionManager kem) throws URISyntaxException {
        this(fileName, mainModuleName, mainProgramsModuleName, false, Sorts.K(), false, false, kem);
    }

    public KoreUtils(String fileName, String mainModuleName, String mainProgramsModuleName, boolean search, Sort sort, boolean heatCoolStrategies, boolean noPrelude, KExceptionManager kem) throws URISyntaxException {
        this.kem = kem;
        File definitionFile = testResource(fileName);
        KompileOptions kompileOptions = new KompileOptions();
        GlobalOptions globalOptions = new GlobalOptions();
        globalOptions.debug = true;
        globalOptions.warnings = GlobalOptions.Warnings.ALL;

        kompileOptions.experimental.heatCoolStrategies = heatCoolStrategies;
        kompileOptions.outerParsing.noPrelude = noPrelude;

        KRunOptions krunOptions = new KRunOptions();
        krunOptions.search = search;

        JavaExecutionOptions javaExecutionOptions = new JavaExecutionOptions();
        FileUtil files = FileUtil.testFileUtil();
        FileSystem fs = new PortableFileSystem(kem, files);

        Kompile kompile = new Kompile(kompileOptions, FileUtil.testFileUtil(), kem, false);
        compiledDef = kompile.run(definitionFile, mainModuleName, mainProgramsModuleName,
                new JavaBackend(kem, files, globalOptions, kompileOptions).steps());

        programParser = compiledDef.getProgramParser(this.kem);

        Map<String, MethodHandle> hookProvider = HookProvider.get(kem);
        InitializeRewriter.InitializeDefinition initializeDefinition = new InitializeRewriter.InitializeDefinition();
        initializeRewriter = new InitializeRewriter(fs, javaExecutionOptions.deterministicFunctions, krunOptions.global, kem, krunOptions.experimental.smt, hookProvider, kompileOptions.transition, krunOptions, files, initializeDefinition);
    }

    public K getParsed(String program, Source source) throws URISyntaxException {
        return getParsed(program, source, null);
    }

    public K getParsed(String program, Source source, String strategy) throws URISyntaxException {
        K parsed = programParser.apply(program, source);
        KRun krun = new KRun(kem, FileUtil.testFileUtil(), true, false);

        Map<KToken, K> map = new HashMap<>();
        map.put(KToken("$PGM", Sorts.KConfigVar()), parsed);

        BiFunction<String, Source, K> strategyParser = compiledDef.getParser(
                compiledDef.programParsingModuleFor(compiledDef.mainSyntaxModuleName(), kem).get(), KORE.Sort("Strategy"), kem);

        if (strategy != null)
            map.put(KToken("$STRATEGY", Sorts.KConfigVar()), strategyParser.apply(strategy, Source.apply("given strategy")));
        return krun.plugConfigVars(compiledDef, map);
    }

    public K stepRewrite(K parsedPgm, Optional<Integer> depth) {
        K kResult = initializeRewriter.apply(Pair.of(compiledDef.executionModule(), null)).execute(parsedPgm, depth).k();
        return kResult;
    }

    public Rewriter getRewriter() {
        rewriter = initializeRewriter.apply(Pair.of(compiledDef.executionModule(), null));
        return rewriter;
    }


    public Module getUnparsingModule() {
        return compiledDef.getExtensionModule(compiledDef.languageParsingModule());
    }

}
