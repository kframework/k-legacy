// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.backend.java.builtins;

import org.kframework.backend.java.kil.TermContext;


/**
 * Table of {@code public static} methods for builtin IO operations.
 */
public class Stuck {
    public static BoolToken stuck(TermContext context) {
        System.err.print(BoolToken.of(context.state().isStuck));
        return BoolToken.of(context.state().isStuck);
    }
}