// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.krun.modes;

import org.kframework.RewriterResult;
import org.kframework.rewriter.Rewriter;
import org.kframework.attributes.Source;
import org.kframework.builtin.BooleanUtils;
import org.kframework.definition.Rule;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kore.K;
import org.kframework.kore.KORE;
import org.kframework.krun.KRun;
import org.kframework.krun.KRunOptions;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import scala.Tuple2;

import java.util.Optional;


/**
 * Execution Mode for Conventional KRun
 */
public class KRunExecutionMode implements ExecutionMode {

    private final KRunOptions kRunOptions;
    private final KExceptionManager kem;
    private final FileUtil files;

    public KRunExecutionMode(KRunOptions kRunOptions, KExceptionManager kem, FileUtil files) {
        this.kRunOptions = kRunOptions;
        this.kem = kem;
        this.files = files;
    }

    @Override
    public Object execute(K k, Rewriter rewriter, CompiledDefinition compiledDefinition) {
        Rule rulePattern = null;
        if (kRunOptions.pattern != null) {
            rulePattern = KRun.compilePattern(files, kem, kRunOptions.pattern, kRunOptions, compiledDefinition, Source.apply("<command line>"));
        }
        if (kRunOptions.search()) {
            if (rulePattern == null) {
                rulePattern = new Rule(KORE.KVariable("X"), BooleanUtils.TRUE, BooleanUtils.TRUE, KORE.Att());
                return rewriter.search(k, Optional.ofNullable(kRunOptions.depth), Optional.ofNullable(kRunOptions.bound), rulePattern, kRunOptions.searchType(), false);
            }
            else {
                return rewriter.search(k, Optional.ofNullable(kRunOptions.depth), Optional.ofNullable(kRunOptions.bound), rulePattern, kRunOptions.searchType(), true);
            }
        }
        if (kRunOptions.exitCodePattern != null) {
            Rule exitCodePattern = KRun.compilePattern(files, kem, kRunOptions.exitCodePattern, kRunOptions, compiledDefinition, Source.apply("<command line: --exit-code>"));
            Tuple2<RewriterResult, K> res;
            if (rulePattern != null) {
                res = rewriter.executeAndMatch(k, Optional.ofNullable(kRunOptions.depth), rulePattern);
                return new Tuple2<>(res._2(), KRun.getExitCode(kem, rewriter.match(res._1().k(), exitCodePattern)));
            }
            res = rewriter.executeAndMatch(k, Optional.ofNullable(kRunOptions.depth), exitCodePattern);
            return Tuple2.apply(res._1().k(), KRun.getExitCode(kem, res._2()));
        }
        if (rulePattern != null) {
            Tuple2<RewriterResult, K> res = rewriter.executeAndMatch(k, Optional.ofNullable(kRunOptions.depth), rulePattern);
            return res;
        }
        return rewriter.execute(k, Optional.ofNullable(kRunOptions.depth)).k();
    }
}
