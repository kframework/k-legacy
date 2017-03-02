// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.parser;


import org.kframework.minikore.interfaces.PatternInterface;
import org.kframework.utils.errorsystem.ParseFailedException;

import java.io.Serializable;
import java.util.Set;

public class ParseResult implements Serializable {
    public final PatternInterface.Pattern ast;
    public final Set<ParseFailedException> warnings;

    public ParseResult(PatternInterface.Pattern ast, Set<ParseFailedException> warnings) {
        this.ast = ast;
        this.warnings = warnings;
    }
}
