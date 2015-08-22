// Copyright (c) 2012-2015 K Team. All Rights Reserved.
package org.kframework.krun;

import com.google.common.collect.ImmutableList;
import com.google.inject.Inject;
import com.google.inject.Module;
import com.google.inject.Provider;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.krun.modes.ExecutionMode;
import org.kframework.main.FrontEnd;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.file.JarInfo;
import org.kframework.utils.file.KompiledDir;
import org.kframework.utils.file.TTYInfo;
import org.kframework.utils.inject.CommonModule;
import org.kframework.utils.inject.DefinitionLoadingModule;
import org.kframework.utils.inject.DefinitionScope;
import org.kframework.utils.inject.JCommanderModule;
import org.kframework.utils.inject.JCommanderModule.ExperimentalUsage;
import org.kframework.utils.inject.JCommanderModule.Usage;
import org.kframework.utils.inject.Main;

import java.io.File;
import java.util.List;
import java.util.function.Function;

public class KRunFrontEnd extends FrontEnd {

    public static List<Module> getDefinitionSpecificModules() {
        return ImmutableList.<Module>of(
                new KRunModule.CommonModule(),
                new DefinitionLoadingModule()
        );
    }

    public static List<Module> getModules(List<Module> definitionSpecificModules) {
        return ImmutableList.<Module>of(
                new KRunModule(),
                new CommonModule(),
                new JCommanderModule(),
                new KRunModule.MainExecutionContextModule(definitionSpecificModules));
    }


    private final KExceptionManager kem;
    private final KRunOptions krunOptions;
    private final FileUtil files;
    private final Provider<CompiledDefinition> compiledDef;
    private final Provider<Function<org.kframework.definition.Module, org.kframework.Rewriter>> initializeRewriter;
    private final Provider<ExecutionMode> executionMode;
    private final TTYInfo tty;
    private final DefinitionScope scope;
    private final Provider<File> kompiledDir;

    /**
     * @return true if the application completed normally; false otherwise
     */
    public int run() {
        scope.enter(kompiledDir.get());
        try {
            return new KRun(kem, files, tty.stdin).run(compiledDef.get(),
                    krunOptions,
                    initializeRewriter.get(),
                    executionMode.get());
        } finally {
            scope.exit();
        }
    }

    @Inject
    KRunFrontEnd(
            GlobalOptions options,
            @Usage String usage,
            @ExperimentalUsage String experimentalUsage,
            KExceptionManager kem,
            JarInfo jarInfo,
            @Main FileUtil files,
            DefinitionScope scope,
            @Main(KompiledDir.class) Provider<File> kompiledDir,
            KRunOptions krunOptions,
            @Main Provider<CompiledDefinition> compiledDef,
            @Main Provider<Function<org.kframework.definition.Module, org.kframework.Rewriter>> initializeRewriter,
            @Main Provider<ExecutionMode> executionMode,
            TTYInfo tty) {
        super(kem, options, usage, experimentalUsage, jarInfo, files);
        this.kem = kem;
        this.krunOptions = krunOptions;
        this.files = files;
        this.compiledDef = compiledDef;
        this.initializeRewriter = initializeRewriter;
        this.executionMode = executionMode;
        this.tty = tty;
        this.scope = scope;
        this.kompiledDir = kompiledDir;
    }
}
