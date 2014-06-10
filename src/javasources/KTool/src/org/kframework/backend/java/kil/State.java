// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.backend.java.kil;

import java.util.HashMap;
import java.util.Map;

import org.kframework.backend.java.symbolic.Transformer;
import org.kframework.backend.java.symbolic.Visitor;
import org.kframework.kil.ASTNode;
import org.kframework.krun.api.io.FileSystem;

import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Table;

/**
 * An object containing context specific to a particular configuration.
 */
public class State {

    private final Definition def;
    private final FileSystem fs;

    public State(Definition def) {
        this(def, null);
    }
    
    public State(Definition def, FileSystem fs) {
        this.def = def;
        this.fs = fs;
    }

    public Definition definition() {
        return def;
    }

    public FileSystem fileSystem() {
        return fs;
    }
}
