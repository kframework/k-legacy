// Copyright (c) 2012-2016 K Team. All Rights Reserved.
package org.kframework.kast;

import org.kframework.attributes.Source;
import org.kframework.kompile.KompileMetaInfo;
import org.kframework.kore.K;
import org.kframework.kore.KORE;
import org.kframework.main.FrontEnd;
import org.kframework.unparser.ToKast;
import org.kframework.utils.Stopwatch;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;

import java.io.Reader;
import java.util.Map;

public class KastFrontEnd extends FrontEnd {

    private final KastOptions options;
    private final Stopwatch sw;
    private final KExceptionManager kem;
    private final Map<String, String> env;
    private final FileUtil files;
    private final KompileMetaInfo kompileMetaInfo;

    public KastFrontEnd(
            KastOptions options,
            Stopwatch sw,
            KExceptionManager kem,
            Map<String, String> env,
            FileUtil files,
            KompileMetaInfo metaInfo) {
        super(kem, options.global, files);
        this.options = options;
        this.sw = sw;
        this.kem = kem;
        this.env = env;
        this.files = files;
        this.kompileMetaInfo = metaInfo;
    }

    /**
     *
     * @return true if the application terminated normally; false otherwise
     */
    @Override
    public int run() {
        Reader stringToParse = options.stringToParse();
        Source source = options.source();

        org.kframework.kore.Sort sort = options.sort;
        if (sort == null) {
            if (env.get("KRUN_SORT") != null) {
                sort = KORE.Sort(env.get("KRUN_SORT"));
            } else {
                sort = KORE.Sort(kompileMetaInfo.programStartSymbol);
            }
        }
        String moduleName = options.module == null ? kompileMetaInfo.mainSyntaxModuleName : options.module;
        K parsed = Kast.parseWithModuleParser(FileUtil.read(stringToParse),source, sort, moduleName, files ,kem);
        System.out.println(ToKast.apply(parsed));
        sw.printTotal("Total");
        return 0;
    }
}
