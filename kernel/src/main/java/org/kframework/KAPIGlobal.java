package org.kframework;

import org.kframework.kompile.KompileOptions;
import org.kframework.krun.KRunOptions;
import org.kframework.krun.api.io.FileSystem;
import org.kframework.krun.ioserver.filesystem.portable.PortableFileSystem;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.options.SMTOptions;

/**
 * Created by daejunpark on 8/14/16.
 */
public class KapiGlobal {
    public GlobalOptions globalOptions;
    public KompileOptions kompileOptions;
    public KRunOptions kRunOptions;
    public SMTOptions smtOptions;

    public KExceptionManager kem;
    public FileUtil files;
    public FileSystem fs;

    public KapiGlobal() {
        this.globalOptions = new GlobalOptions();

        this.kompileOptions = new KompileOptions();
        this.kompileOptions.global = globalOptions;

        this.kRunOptions = new KRunOptions();
        this.kRunOptions.global = globalOptions;

        this.smtOptions = kRunOptions.experimental.smt;

        this.kem = new KExceptionManager(globalOptions);
        this.files = FileUtil.get(globalOptions, System.getenv());
        this.fs = new PortableFileSystem(kem, files);
    }

    public void setVerbose(boolean v) {
        this.globalOptions.verbose = v;
    }

    public void setDebug(boolean v) {
        this.globalOptions.debug = v;
    }

    public void setWarnings(GlobalOptions.Warnings v) {
        this.globalOptions.warnings = v;
    }

    public void setTransitions(java.util.List<String> v) {
        this.kompileOptions.transition = v;
    }

    public void setIncludes(java.util.List<String> v) {
        this.kompileOptions.outerParsing.includes = v;
    }

    public void setNoPrelude(boolean v) {
        this.kompileOptions.outerParsing.noPrelude = v;
    }

    public void setSmtPrelude(String v) {
        this.smtOptions.smtPrelude = v;
    }
}
