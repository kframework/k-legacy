// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.krun;

import org.kframework.definition.Rule;
import org.kframework.kore.K;

/**
 * Created by manasvi on 9/4/15.
 * Object Containing the results of a search operation.
 */
public class SearchResult {
    private Rule parsedRule;


    private K solutions;


    public SearchResult(K solutions, Rule parsedRule) {
        this.parsedRule = parsedRule;
        this.solutions = solutions;

    }


    public K getSolutions() {
        return solutions;
    }


    public Rule getParsedRule() {
        return parsedRule;
    }

}
