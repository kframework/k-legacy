// Copyright (c) 2015 K Team. All Rights Reserved.
package org.kframework.kore.strategies;

import com.google.common.base.Stopwatch;
import com.google.inject.Inject;
import org.kframework.Rewriter;
import org.kframework.Strategy;
import org.kframework.definition.Rule;
import org.kframework.kore.K;
import org.kframework.kore.KVariable;
import org.kframework.krun.KRunOptions;

import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Created by dwightguth on 5/8/15.
 */
public class Execute implements Strategy<K> {

    private final KRunOptions krunOptions;

    @Inject
    public Execute(KRunOptions krunOptions) {
        this.krunOptions = krunOptions;
    }

    @Override
    public K execute(K k, Rewriter rewriter) {
        k = rewriter.convert(k);
        Stopwatch stopwatch = Stopwatch.createStarted();
        int step = 0;
        List<? extends Rule> rules = rewriter.rules();
        while (true) {
            List<? extends Map<? extends KVariable, ? extends K>> res = Collections.emptyList();
            for (Rule r : rules) {
                res = rewriter.match(k, false, r);
                if (res.isEmpty()) continue;
                step++;
                k = rewriter.substitute(res.get(0), r);
                break;
            }
            if (res.isEmpty()) {
                stopwatch.stop();
                if (krunOptions.experimental.statistics) {
                    System.err.println("[" + step + ", " + stopwatch + "]");
                }

                return k;
            }
        }
    }
}
