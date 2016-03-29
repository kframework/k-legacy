// Copyright (c) 2016 K Team. All Rights Reserved.
package org.kframework;

import com.google.inject.Provider;
import org.kframework.RewriterResult;
import org.kframework.backend.java.symbolic.InitializeRewriter;
import org.kframework.backend.java.symbolic.JavaExecutionOptions;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kompile.KompileOptions;
import org.kframework.kore.K;
import org.kframework.krun.KRun;
import org.kframework.krun.KRunOptions;
import org.kframework.krun.api.io.FileSystem;
import org.kframework.krun.ioserver.filesystem.portable.PortableFileSystem;
import org.kframework.main.GlobalOptions;
import org.kframework.rewriter.Rewriter;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;

import java.lang.invoke.MethodHandle;
import java.util.HashMap;
import java.util.Map;

import static org.kframework.Collections.*;
import static org.kframework.kore.KORE.*;

/**
 * The KORE-based KRun
 */
public class KRunAPI {

    public static RewriterResult run(CompiledDefinition compiledDef) {

        GlobalOptions globalOptions = new GlobalOptions();
        KompileOptions kompileOptions = new KompileOptions();
        KRunOptions krunOptions = new KRunOptions();
        JavaExecutionOptions javaExecutionOptions = new JavaExecutionOptions();

        KExceptionManager kem = new KExceptionManager(globalOptions);
        FileUtil files = FileUtil.testFileUtil();
        boolean ttyStdin = false;

        FileSystem fs = new PortableFileSystem(kem, files);
        Map<String, Provider<MethodHandle>> hookProvider = new HashMap<>();
        InitializeRewriter.InitializeDefinition initializeDefinition = new InitializeRewriter.InitializeDefinition();

        K program = KRun.parseConfigVars(krunOptions, compiledDef, kem, files, ttyStdin);

        Rewriter rewriter = (InitializeRewriter.SymbolicRewriterGlue)
            new InitializeRewriter(
                fs,
                javaExecutionOptions,
                globalOptions,
                kem,
                kompileOptions.experimental.smt,
                hookProvider,
                kompileOptions,
                krunOptions,
                files,
                initializeDefinition)
            .apply(compiledDef.executionModule());

        RewriterResult result = rewriter.execute(program, null);

        return result;
    }

    public static void main(String[] args) {
        CompiledDefinition compiledDef = null;
        run(compiledDef);
    }

}
