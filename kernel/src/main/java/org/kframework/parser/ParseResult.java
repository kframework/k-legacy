package org.kframework.parser;


import org.kframework.kore.K;
import org.kframework.utils.errorsystem.ParseFailedException;

import java.io.Serializable;
import java.util.Set;

public class ParseResult implements Serializable {
    public final K ast;
    public final Set<ParseFailedException> warnings;

    public ParseResult(K ast, Set<ParseFailedException> warnings) {
        this.ast = ast;
        this.warnings = warnings;
    }
}
