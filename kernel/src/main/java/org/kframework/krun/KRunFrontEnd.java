// Copyright (c) 2012-2016 K Team. All Rights Reserved.
package org.kframework.krun;

import org.apache.commons.lang3.tuple.Pair;
import org.kframework.definition.Module;
import org.kframework.definition.ProcessedDefinition;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kompile.KompileMetaInfo;
import org.kframework.krun.modes.ExecutionMode;
import org.kframework.main.FrontEnd;
import org.kframework.main.GlobalOptions;
import org.kframework.minikore.implementation.MiniKore;
import org.kframework.rewriter.Rewriter;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.file.TTYInfo;

import java.io.File;
import java.util.function.Function;

public class KRunFrontEnd extends FrontEnd {

    private final KExceptionManager kem;
    private final KRunOptions krunOptions;
    private final FileUtil files;
    private final KompileMetaInfo kompileMetaInfo;
    private final CompiledDefinition compiledDef;
    private final Function<Pair<Module, MiniKore.Definition>, Rewriter> initializeRewriter;
    private final ExecutionMode executionMode;
    private final TTYInfo tty;
    private final boolean isNailgun;
    private final ProcessedDefinition processedDefinition;

    public KRunFrontEnd(
            GlobalOptions options,
            KExceptionManager kem,
            KRunOptions krunOptions,
            FileUtil files,
            KompileMetaInfo kompileMetaInfo,
            CompiledDefinition compiledDef,
            ProcessedDefinition processedDefinition,
            Function<Pair<Module, MiniKore.Definition>, Rewriter> initializeRewriter,
            ExecutionMode executionMode,
            TTYInfo tty,
            boolean isNailgun) {
        super(kem, options, files);
        this.kem = kem;
        this.krunOptions = krunOptions;
        this.files = files;
        this.kompileMetaInfo = kompileMetaInfo;
        this.compiledDef = compiledDef;
        this.initializeRewriter = initializeRewriter;
        this.executionMode = executionMode;
        this.tty = tty;
        this.isNailgun = isNailgun;
        this.processedDefinition = processedDefinition;
    }

    /**
     * @return the exit code returned from executing krun.
     */
    public int run() {
        for (int i = 0; i < krunOptions.experimental.profile - 1; i++) {
            new KRun(kem, files, tty.stdin, isNailgun).run(kompileMetaInfo, compiledDef, processedDefinition,
                    krunOptions,
                    initializeRewriter, executionMode);
        }
        return new KRun(kem, files, tty.stdin, isNailgun).run(kompileMetaInfo, compiledDef, processedDefinition,
                krunOptions,
                initializeRewriter, executionMode);
    }
}
