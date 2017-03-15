// Copyright (c) 2014-2016 K Team. All Rights Reserved.

package org.kframework.backend.java.kil;

import org.kframework.KapiGlobal;
import org.kframework.backend.java.kil.KItem.KItemOperations;
import org.kframework.backend.java.symbolic.BuiltinFunction;
import org.kframework.backend.java.symbolic.Equality.EqualityOperations;
import org.kframework.backend.java.symbolic.JavaExecutionOptions;
import org.kframework.backend.java.symbolic.SMTOperations;
import org.kframework.backend.java.symbolic.Stage;
import org.kframework.backend.java.util.Z3Wrapper;
import org.kframework.kast.Kast;
import org.kframework.krun.KRunOptions;
import org.kframework.krun.api.io.FileSystem;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.options.SMTOptions;

import java.io.Serializable;
import java.lang.invoke.MethodHandle;
import java.util.Map;

public class GlobalContext implements Serializable {
    private Definition def;
    private transient Kast kastParser = null;
    public final transient FileSystem fs;
    public final Stage stage;
    public final transient EqualityOperations equalityOps;
    public final transient SMTOperations constraintOps;
    public final transient KItemOperations kItemOps;
    public final transient KRunOptions krunOptions;
    public final transient KExceptionManager kem;
    private final transient Map<String, MethodHandle> hookProvider;
    public final transient FileUtil files;
    public final transient GlobalOptions globalOptions;

    public GlobalContext(
            FileSystem fs,
            boolean deterministicFunctions,
            GlobalOptions globalOptions,
            KRunOptions krunOptions,
            KExceptionManager kem,
            SMTOptions smtOptions,
            Map<String, MethodHandle> hookProvider,
            FileUtil files,
            Stage stage) {
        this.fs = fs;
        this.globalOptions = globalOptions;
        this.krunOptions = krunOptions;
        this.kem = kem;
        this.hookProvider = hookProvider;
        this.files = files;
        this.equalityOps = new EqualityOperations(() -> def);
        this.constraintOps = new SMTOperations(() -> def, smtOptions, new Z3Wrapper(smtOptions, kem, globalOptions, files));
        this.kItemOps = new KItemOperations(stage, deterministicFunctions, kem, this::builtins, globalOptions);
        this.stage = stage;
    }

    public GlobalContext(
            KapiGlobal g,
            Map<String, MethodHandle> hookProvider,
            Stage stage) {
        this(g.fs, g.deterministicFunctions, g.globalOptions, g.kRunOptions, g.kem, g.smtOptions, hookProvider, g.files, stage);
    }

    private transient BuiltinFunction builtinFunction;
    private BuiltinFunction builtins() {
        BuiltinFunction b = builtinFunction;
        if (b == null) {
            b = new BuiltinFunction(def, hookProvider, kem, stage);
            builtinFunction = b;
        }
        return b;
    }

    public void setDefinition(Definition def) {
        this.def = def;
    }

    public Definition getDefinition() {
        return def;
    }

    public void setKastParser(Kast kastParser) {
        this.kastParser = kastParser;
    }

    public Kast getKastParser() { return this.kastParser; }

}
