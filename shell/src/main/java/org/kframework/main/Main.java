// Copyright (c) 2014-2016 K Team. All Rights Reserved.
package org.kframework.main;

import com.beust.jcommander.JCommander;
import com.google.common.collect.ImmutableSet;
import com.martiansoftware.nailgun.NGContext;
import org.apache.commons.lang3.tuple.Pair;
import org.fusesource.jansi.AnsiConsole;
import org.kframework.HookProvider;
import org.kframework.Kapi;
import org.kframework.backend.Backends;
import org.kframework.backend.java.symbolic.InitializeRewriter;
import org.kframework.backend.java.symbolic.JavaBackend;
import org.kframework.backend.java.symbolic.JavaExecutionOptions;
import org.kframework.backend.java.symbolic.ProofExecutionMode;
import org.kframework.definition.Module;
import org.kframework.definition.ProcessedDefinition;
import org.kframework.kale.KaleBackend;
//import org.kframework.kale.KaleRewriter;
import org.kframework.kale.KaleRewriter;
import org.kframework.kast.KastFrontEnd;
import org.kframework.kast.KastOptions;
import org.kframework.kdep.KDepFrontEnd;
import org.kframework.kdep.KDepOptions;
import org.kframework.kdoc.KDocOptions;
import org.kframework.keq.KeqOptions;
import org.kframework.kil.loader.Context;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kompile.KompileFrontEnd;
import org.kframework.kompile.KompileMetaInfo;
import org.kframework.kompile.KompileOptions;
import org.kframework.frontend.compile.Backend;
import org.kframework.krun.KRunFrontEnd;
import org.kframework.krun.KRunOptions;
import org.kframework.krun.api.io.FileSystem;
import org.kframework.krun.ioserver.filesystem.portable.PortableFileSystem;
import org.kframework.krun.modes.DebugMode.DebugExecutionMode;
import org.kframework.krun.modes.ExecutionMode;
import org.kframework.krun.modes.KRunExecutionMode;
import org.kframework.kserver.KServerFrontEnd;
import org.kframework.kserver.KServerOptions;
import org.kframework.ktest.CmdArgs.KTestOptions;
import org.kframework.ktest.KTestFrontEnd;
import org.kframework.rewriter.Rewriter;
import org.kframework.utils.BinaryLoader;
import org.kframework.utils.Stopwatch;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.file.JarInfo;
import org.kframework.utils.file.TTYInfo;
import org.kframework.utils.inject.CommonModule;
import org.kframework.utils.inject.DefinitionLoadingModule;
import org.kframework.utils.inject.JCommanderModule;
import org.kframework.utils.inject.OuterParsingModule;

import java.io.File;
import java.io.IOException;
import java.lang.invoke.MethodHandle;
import java.util.Arrays;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

public class Main {

    /**
     * @param args - the running arguments for the K3 tool. First argument must be one of the following: kompile|kast|krun.
     * @throws IOException when loadDefinition fails
     */
    public static void main(String[] args) {
        AnsiConsole.systemInstall();
        if (args.length >= 1) {
            String[] args2 = Arrays.copyOfRange(args, 1, args.length);
            int result = runApplication(args[0], args2, new File("."), System.getenv());
            AnsiConsole.systemUninstall();
            System.exit(result);
        }
        AnsiConsole.systemUninstall();
        invalidJarArguments();
    }

    public static void usage(GlobalOptions globalOptions, String usage, String experimentalUsage, JarInfo jarInfo) {
        if (globalOptions.help) {
            System.out.print(usage);
            System.exit(0);
        } else if (globalOptions.helpExperimental) {
            System.out.print(experimentalUsage);
            System.exit(0);
        } else if (globalOptions.version) {
            jarInfo.printVersionMessage();
            System.exit(0);
        }
    }

    public static int runApplication(String toolName, String[] args, File workingDir, Map<String, String> env) {

        if (toolName.equals("-kompile")) {
            Tool tool = Tool.KOMPILE;

            // basics
            KompileOptions kompileOptions = new KompileOptions();
            KExceptionManager kem = new KExceptionManager(kompileOptions.global);
            Stopwatch sw = new Stopwatch(kompileOptions.global);
            BinaryLoader loader = new BinaryLoader(kem);
            JarInfo jarInfo = new JarInfo(kem);

            // parsing options
            Set<Object> options = ImmutableSet.of(kompileOptions);
            Set<Class<?>> experimentalOptions = ImmutableSet.of(kompileOptions.experimental.getClass(),      // KompileOptions.Experimental.class
                    kompileOptions.experimental.smt.getClass()); // SMTOptions.class
            JCommander jc = JCommanderModule.jcommander(args, tool, options, experimentalOptions, kem, sw);
            String usage = JCommanderModule.usage(jc);
            String experimentalUsage = JCommanderModule.experimentalUsage(jc);
            usage(kompileOptions.global, usage, experimentalUsage, jarInfo);

            // directories
            File tempDir = CommonModule.tempDir(workingDir, tool);
            File definitionDir = OuterParsingModule.definitionDir(workingDir, kompileOptions.outerParsing);
            File kompiledDir = OuterParsingModule.kompiledDir(definitionDir, kompileOptions.outerParsing, workingDir, tempDir);
            FileUtil files = new FileUtil(tempDir, definitionDir, workingDir, kompiledDir, kompileOptions.global, env);

            // kompile
            Backend koreBackend;
            if (kompileOptions.backend.equals(Backends.JAVA)) {
                koreBackend = new JavaBackend(kem, files, kompileOptions.global, kompileOptions);
            } else if (kompileOptions.backend.equals(Backends.KALE)) {
                koreBackend = new KaleBackend(kompileOptions, kem);
            } else
                throw new AssertionError("Backend not hooked to the shell.");
            KompileFrontEnd frontEnd = new KompileFrontEnd(kompileOptions, koreBackend, sw, kem, loader, files);

            return runApplication(frontEnd, kem);
        }

        if (toolName.equals("-kdoc")) {
            Tool tool = Tool.KDOC;

            // basics
            KDocOptions kDocOptions = new KDocOptions();
            KExceptionManager kem = new KExceptionManager(kDocOptions.global);
            Stopwatch sw = new Stopwatch(kDocOptions.global);
            JarInfo jarInfo = new JarInfo(kem);

            // parsing options
            Set<Object> options = ImmutableSet.of(kDocOptions);
            Set<Class<?>> experimentalOptions = ImmutableSet.of();
            JCommander jc = JCommanderModule.jcommander(args, tool, options, experimentalOptions, kem, sw);
            String usage = JCommanderModule.usage(jc);
            String experimentalUsage = JCommanderModule.experimentalUsage(jc);
            usage(kDocOptions.global, usage, experimentalUsage, jarInfo);

            return 0;
            /* TODO: complete implementation
            // directories
            File tempDir = CommonModule.tempDir(workingDir, tool);
            File definitionDir = OuterParsingModule.definitionDir(workingDir, kDocOptions.outerParsing);
            File kompiledDir = OuterParsingModule.kompiledDir(definitionDir, kDocOptions.outerParsing, workingDir, tempDir); // TODO: handle when only --directory option is provided
            FileUtil files = new FileUtil(tempDir, definitionDir, workingDir, kompiledDir, kDocOptions.global, env);

            // kdep
            KDocFrontEnd frontEnd = new KDocFrontEnd(kDocOptions, kem, kDocOptions.global, files, null); // TODO: correct PosterBackend

            return runApplication(frontEnd, kem);
             */
        }

        if (toolName.equals("-kdep")) {
            Tool tool = Tool.KDEP;

            // basics
            KDepOptions kDepOptions = new KDepOptions();
            KExceptionManager kem = new KExceptionManager(kDepOptions.global);
            Stopwatch sw = new Stopwatch(kDepOptions.global);
            JarInfo jarInfo = new JarInfo(kem);

            // parsing options
            Set<Object> options = ImmutableSet.of(kDepOptions);
            Set<Class<?>> experimentalOptions = ImmutableSet.of();
            JCommander jc = JCommanderModule.jcommander(args, tool, options, experimentalOptions, kem, sw);
            String usage = JCommanderModule.usage(jc);
            String experimentalUsage = JCommanderModule.experimentalUsage(jc);
            usage(kDepOptions.global, usage, experimentalUsage, jarInfo);

            // directories
            File tempDir = CommonModule.tempDir(workingDir, tool);
            File definitionDir = OuterParsingModule.definitionDir(workingDir, kDepOptions.outerParsing);
            File kompiledDir = OuterParsingModule.kompiledDir(definitionDir, kDepOptions.outerParsing, workingDir, tempDir);
            FileUtil files = new FileUtil(tempDir, definitionDir, workingDir, kompiledDir, kDepOptions.global, env);

            // kdep
            KDepFrontEnd frontEnd = new KDepFrontEnd(kDepOptions.outerParsing, kem, kDepOptions.global, sw, files);

            return runApplication(frontEnd, kem);
        }

        if (toolName.equals("-krun")) {
            Tool tool = Tool.KRUN;

            // basics
            KRunOptions kRunOptions = new KRunOptions();
            JavaExecutionOptions javaExecutionOptions = new JavaExecutionOptions();
            KExceptionManager kem = new KExceptionManager(kRunOptions.global);
            Stopwatch sw = new Stopwatch(kRunOptions.global);
            BinaryLoader loader = new BinaryLoader(kem);
            JarInfo jarInfo = new JarInfo(kem);
            TTYInfo ttyInfo = CommonModule.ttyInfo(env);

            // parsing options
            Set<Object> options = ImmutableSet.of(kRunOptions, javaExecutionOptions);
            Set<Class<?>> experimentalOptions = ImmutableSet.of(kRunOptions.experimental.getClass(),        // KRunOptions.Experimental.class
                    kRunOptions.experimental.smt.getClass(),    // SMTOptions.class
                    javaExecutionOptions.getClass());           // JavaExecutionOptions.class
            JCommander jc = JCommanderModule.jcommander(args, tool, options, experimentalOptions, kem, sw);
            String usage = JCommanderModule.usage(jc);
            String experimentalUsage = JCommanderModule.experimentalUsage(jc);
            usage(kRunOptions.global, usage, experimentalUsage, jarInfo);

            // directories
            File tempDir = CommonModule.tempDir(workingDir, tool);
            File definitionDir = DefinitionLoadingModule.directory(kRunOptions.configurationCreation.definitionLoading, workingDir, kem, env);
            File kompiledDir = DefinitionLoadingModule.definition(definitionDir, kem);
            FileUtil files = new FileUtil(tempDir, definitionDir, workingDir, kompiledDir, kRunOptions.global, env);
            FileSystem fs = new PortableFileSystem(kem, files);

            // loading kompiled definition
            Context context = null; // DefinitionLoadingModule.context(loader, kRunOptions.configurationCreation.definitionLoading, kRunOptions.global, sw, kem, files, kRunOptions); // TODO: check if 'context.bin' exists
            KompileMetaInfo kompileMetaInfo = DefinitionLoadingModule.kompilemetaInfo(files);
            CompiledDefinition compiledDef = DefinitionLoadingModule.koreDefinition(loader, files);
            ProcessedDefinition processedDefinition = DefinitionLoadingModule.miniKoreDefinition(loader, files);
            KompileOptions kompileOptions = DefinitionLoadingModule.kompileOptions(context, compiledDef, files);

            // krun

            Function<Module, Rewriter> initializeRewriter;
            Function<Pair<Module, org.kframework.kore.Definition>, Rewriter> intializeMiniKoreRewriter;
            if (kompileOptions.backend.equals(Backends.JAVA)) {
                //
                Map<String, MethodHandle> hookProvider = HookProvider.get(kem);
                InitializeRewriter.InitializeDefinition initializeDefinition = new InitializeRewriter.InitializeDefinition();
                intializeMiniKoreRewriter = new InitializeRewriter(fs, javaExecutionOptions.deterministicFunctions,
                        kRunOptions.global, kem, kRunOptions.experimental.smt, hookProvider, kompileOptions.transition,
                        kRunOptions, files, initializeDefinition);
            } else if (kompileOptions.backend.equals(Backends.KALE)) {
                initializeRewriter = null;
                intializeMiniKoreRewriter = (x -> new KaleRewriter(x.getKey(), x.getRight()));
            } else {
                throw new AssertionError("Backend not hooked to the shell.");
            }

            ExecutionMode executionMode;
            boolean isProofMode = kRunOptions.experimental.prove != null;
            boolean isDebugMode = kRunOptions.experimental.debugger();
            assert !(isProofMode && isDebugMode); // TODO: generate error messages for multiple tool activations
            if (isProofMode) {
                executionMode = new ProofExecutionMode(kem, kRunOptions, sw, files, kRunOptions.global);
            } else if (isDebugMode) {
                executionMode = new DebugExecutionMode(kRunOptions, kem, files, 500, fs);
            } else {
                executionMode = new KRunExecutionMode(kRunOptions, kem, files);
            }

            KRunFrontEnd frontEnd = new KRunFrontEnd(kRunOptions.global, kem, kRunOptions, files, kompileMetaInfo,
                    compiledDef, processedDefinition, intializeMiniKoreRewriter, executionMode, ttyInfo, isNailgun);

            return runApplication(frontEnd, kem);
        }

        if (toolName.equals("-kast")) {
            Tool tool = Tool.KAST;

            // basics
            KastOptions kastOptions = new KastOptions();
            KExceptionManager kem = new KExceptionManager(kastOptions.global);
            Stopwatch sw = new Stopwatch(kastOptions.global);
            BinaryLoader loader = new BinaryLoader(kem);
            JarInfo jarInfo = new JarInfo(kem);

            // parsing options
            Set<Object> options = ImmutableSet.of(kastOptions);
            Set<Class<?>> experimentalOptions = ImmutableSet.of(kastOptions.experimental.getClass());
            JCommander jc = JCommanderModule.jcommander(args, tool, options, experimentalOptions, kem, sw);
            String usage = JCommanderModule.usage(jc);
            String experimentalUsage = JCommanderModule.experimentalUsage(jc);
            usage(kastOptions.global, usage, experimentalUsage, jarInfo);

            // directories
            File tempDir = CommonModule.tempDir(workingDir, tool);
            File definitionDir = DefinitionLoadingModule.directory(kastOptions.definitionLoading, workingDir, kem, env);
            File kompiledDir = DefinitionLoadingModule.definition(definitionDir, kem);
            FileUtil files = new FileUtil(tempDir, definitionDir, workingDir, kompiledDir, kastOptions.global, env);
            KompileMetaInfo kompileMetaInfo = DefinitionLoadingModule.kompilemetaInfo(files);

            kastOptions.setFiles(files);
            KastFrontEnd frontEnd = new KastFrontEnd(kastOptions, sw, kem, env, files, kompileMetaInfo);

            return runApplication(frontEnd, kem);
        }

        if (toolName.equals("-kserver")) {
            Tool tool = Tool.KSERVER;

            // basics
            KServerOptions kServerOptions = new KServerOptions();
            KExceptionManager kem = new KExceptionManager(kServerOptions.global);
            Stopwatch sw = new Stopwatch(kServerOptions.global);
            JarInfo jarInfo = new JarInfo(kem);

            // parsing options
            Set<Object> options = ImmutableSet.of(kServerOptions);
            Set<Class<?>> experimentalOptions = ImmutableSet.of();
            JCommander jc = JCommanderModule.jcommander(args, tool, options, experimentalOptions, kem, sw);
            String usage = JCommanderModule.usage(jc);
            String experimentalUsage = JCommanderModule.experimentalUsage(jc);
            usage(kServerOptions.global, usage, experimentalUsage, jarInfo);

            // directories
            File tempDir = CommonModule.tempDir(workingDir, tool);
            File definitionDir = null;
            File kompiledDir = null;
            FileUtil files = new FileUtil(tempDir, definitionDir, workingDir, kompiledDir, kServerOptions.global, env);

            KServerFrontEnd frontEnd = new KServerFrontEnd(kem, kServerOptions, files);

            return runApplication(frontEnd, kem);
        }

        if (toolName.equals("-ktest")) {
            Tool tool = Tool.KTEST;

            // basics
            KTestOptions kTestOptions = new KTestOptions();
            KExceptionManager kem = new KExceptionManager(kTestOptions.getGlobal());
            Stopwatch sw = new Stopwatch(kTestOptions.getGlobal());
            JarInfo jarInfo = new JarInfo(kem);

            // parsing options
            Set<Object> options = ImmutableSet.of(kTestOptions);
            Set<Class<?>> experimentalOptions = ImmutableSet.of();
            JCommander jc = JCommanderModule.jcommander(args, tool, options, experimentalOptions, kem, sw);
            String usage = JCommanderModule.usage(jc);
            String experimentalUsage = JCommanderModule.experimentalUsage(jc);
            usage(kTestOptions.getGlobal(), usage, experimentalUsage, jarInfo);

            // directories
            File tempDir = CommonModule.tempDir(workingDir, tool);
            File definitionDir = null;
            File kompiledDir = null;
            FileUtil files = new FileUtil(tempDir, definitionDir, workingDir, kompiledDir, kTestOptions.getGlobal(), env);

            KTestFrontEnd frontEnd = new KTestFrontEnd(kTestOptions, kem, kTestOptions.getGlobal(), env, files);

            return runApplication(frontEnd, kem);
        }

        if (toolName.equals("-kpp")) {
            Tool tool = Tool.KPP;

            // basics
            GlobalOptions globalOptions = new GlobalOptions();
            KExceptionManager kem = new KExceptionManager(globalOptions);

            // directories
            File tempDir = CommonModule.tempDir(workingDir, tool);
            File definitionDir = null;
            File kompiledDir = null;
            FileUtil files = new FileUtil(tempDir, definitionDir, workingDir, kompiledDir, globalOptions, env);

            KppFrontEnd frontEnd = new KppFrontEnd(kem, globalOptions, files, args);

            return runApplication(frontEnd, kem);
        }

        if (toolName.equals("-keq")) {
            Tool tool = Tool.KEQ;

            // basics
            KeqOptions keqOptions = new KeqOptions();
            KExceptionManager kem = new KExceptionManager(keqOptions.global);
            Stopwatch sw = new Stopwatch(keqOptions.global);
            BinaryLoader loader = new BinaryLoader(kem);
            JarInfo jarInfo = new JarInfo(kem);

            // parsing options
            Set<Object> options = ImmutableSet.of(keqOptions);
            Set<Class<?>> experimentalOptions = ImmutableSet.of();
            JCommander jc = JCommanderModule.jcommander(args, tool, options, experimentalOptions, kem, sw);
            String usage = JCommanderModule.usage(jc);
            String experimentalUsage = JCommanderModule.experimentalUsage(jc);
            usage(keqOptions.global, usage, experimentalUsage, jarInfo);

            File def0File = FileUtil.resolveWorkingDirectory(new File(keqOptions.def0), workingDir);
            File def1File = FileUtil.resolveWorkingDirectory(new File(keqOptions.def1), workingDir);
            File def2File = FileUtil.resolveWorkingDirectory(new File(keqOptions.def2), workingDir);
            //
            String prelude = FileUtil.resolveWorkingDirectory(new File(keqOptions.smt.smtPrelude), workingDir).getAbsolutePath();
            //
            String prove1  = FileUtil.resolveWorkingDirectory(new File(keqOptions.prove1), workingDir).getAbsolutePath();
            String prove2  = FileUtil.resolveWorkingDirectory(new File(keqOptions.prove2), workingDir).getAbsolutePath();

            CompiledDefinition compiledDef0 = DefinitionLoadingModule.koreDefinition(loader, new FileUtil(null, null, null, def0File, null, null));
            CompiledDefinition compiledDef1 = DefinitionLoadingModule.koreDefinition(loader, new FileUtil(null, null, null, def1File, null, null));
            CompiledDefinition compiledDef2 = DefinitionLoadingModule.koreDefinition(loader, new FileUtil(null, null, null, def2File, null, null));

            // kequiv
            Kapi.kequiv(compiledDef0, compiledDef1, compiledDef2, prove1, prove2, prelude);

            return 0;
        }

        invalidJarArguments();
        return 1; // not reached
    }

    public static int runApplication(FrontEnd frontEnd, KExceptionManager kem) {
        kem.installForUncaughtExceptions();
        int retval = frontEnd.main();
        return retval;
    }

    private static void invalidJarArguments() {
        System.err.println("The first argument of K3 not recognized. Try -kompile, -kast, -krun, -ktest, -kserver, or -kpp.");
        System.exit(1);
    }

    private static volatile boolean isNailgun;

    public static boolean isNailgun() {
        return isNailgun;
    }

    public static void nailMain(NGContext context) {
        isNailgun = true;
        if (context.getArgs().length >= 1) {
            String[] args2 = Arrays.copyOfRange(context.getArgs(), 1, context.getArgs().length);
            KServerFrontEnd kserver = KServerFrontEnd.instance();
            int result = kserver.run(context.getArgs()[0], args2, new File(context.getWorkingDirectory()), (Map) context.getEnv());
            System.exit(result);
            return;
        }
        invalidJarArguments();
    }

}
