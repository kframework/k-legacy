// Copyright (c) 2014-2015 K Team. All Rights Reserved.
package org.kframework.backend.unparser;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Set;

import org.kframework.kil.Attributes;
import org.kframework.krun.api.KRunGraph;
import org.kframework.krun.api.KRunProofResult;
import org.kframework.krun.api.KRunResult;
import org.kframework.krun.api.KRunState;
import org.kframework.krun.api.SearchResults;
import org.kframework.transformation.Transformation;
import org.kframework.utils.inject.InjectGeneric;

import com.google.inject.Inject;

public class PrintKRunResult implements Transformation<KRunResult, InputStream> {

    @InjectGeneric private Transformation<KRunState, InputStream> statePrinter;
    @InjectGeneric private Transformation<SearchResults, String> searchResultsPrinter;
    @InjectGeneric private Transformation<KRunGraph, String> graphPrinter;

    @Inject
    public PrintKRunResult() {}

    public PrintKRunResult(
            Transformation<KRunState, InputStream> statePrinter,
            Transformation<SearchResults, String> searchResultsPrinter,
            Transformation<KRunGraph, String> graphPrinter) {
        this.statePrinter = statePrinter;
        this.searchResultsPrinter = searchResultsPrinter;
        this.graphPrinter = graphPrinter;
    }

    @Override
    public InputStream run(KRunResult krunResult, Attributes a) {
        if (krunResult instanceof KRunProofResult && ((KRunProofResult<?>) krunResult).isProven()) {
            return new ByteArrayInputStream("true\n".getBytes());
        }
        return print(krunResult, a);
    }

    private InputStream print(Object result, Attributes a) {
        StringBuilder sb = new StringBuilder();
        if (result instanceof KRunState) {
            return statePrinter.run((KRunState)result, a);
        } else if (result instanceof SearchResults) {
            return new ByteArrayInputStream(searchResultsPrinter.run((SearchResults)result, a).getBytes());
        } else if (result instanceof KRunGraph) {
            return new ByteArrayInputStream(graphPrinter.run((KRunGraph)result, a).getBytes());
        } else if (result instanceof Set) {
            int i = 1;
            for (Object o : ((Set<?>)result)) {
                sb.append("Result " + i + ":");
                sb.append(print(o, a));
                i++;
            }
            if (i == 1) {
                sb.append("No results");
            }
        } else {
            assert false : "unexpected output type";
        }
        return new ByteArrayInputStream(sb.toString().getBytes());
    }

    @Override
    public String getName() {
        return "print result of execution";
    }
}
