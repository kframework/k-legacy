// Copyright (c) 2012-2016 K Team. All Rights Reserved.
package org.kframework.kast;

import com.google.inject.Module;
import org.kframework.attributes.Source;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kore.K;
import org.kframework.unparser.ToKast;
import org.kframework.main.FrontEnd;
import org.kframework.utils.Stopwatch;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.file.JarInfo;
import org.kframework.utils.inject.CommonModule;
import org.kframework.utils.inject.JCommanderModule;
import scala.Option;

import java.io.File;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.kframework.kore.KORE.*;

public class KastFrontEnd extends FrontEnd {

    private final KastOptions options;
    private final Stopwatch sw;
    private final KExceptionManager kem;
    private final Map<String, String> env;
    private final File kompiledDir;
    private final CompiledDefinition compiledDef;

    public KastFrontEnd(
            KastOptions options,
            String usage,
            String experimentalUsage,
            Stopwatch sw,
            KExceptionManager kem,
            JarInfo jarInfo,
            Map<String, String> env,
            FileUtil files,
            File kompiledDir,
            CompiledDefinition compiledDef) {
        super(kem, options.global, usage, experimentalUsage, jarInfo, files);
        this.options = options;
        this.sw = sw;
        this.kem = kem;
        this.env = env;
        this.kompiledDir = kompiledDir;
        this.compiledDef = compiledDef;
    }

    /**
     *
     * @return true if the application terminated normally; false otherwise
     */
    @Override
    public int run() {
        Reader stringToParse = options.stringToParse();
        Source source = options.source();

        CompiledDefinition def = compiledDef;
        org.kframework.kore.Sort sort = options.sort;
        if (sort == null) {
            if (env.get("KRUN_SORT") != null) {
                sort = Sort(env.get("KRUN_SORT"));
            } else {
                sort = def.programStartSymbol;
            }
        }
        org.kframework.definition.Module mod;
        if (options.module == null) {
            mod = def.programParsingModuleFor(def.mainSyntaxModuleName(), kem).get();
        } else {
            Option<org.kframework.definition.Module> mod2 = def.programParsingModuleFor(options.module, kem);
            if (mod2.isEmpty()) {
                throw KEMException.innerParserError("Module " + options.module + " not found. Specify a module with -m.");
            }
            mod = mod2.get();
        }
        K parsed = def.getParser(mod, sort, kem).apply(FileUtil.read(stringToParse), source);
        System.out.println(ToKast.apply(parsed));
        sw.printTotal("Total");
        return 0;
    }
}
