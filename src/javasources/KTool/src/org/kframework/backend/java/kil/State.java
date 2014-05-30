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
public class State extends JavaSymbolicObject {

    private static Map<Definition, State> cache1 = new HashMap<Definition, State>();
    private static Table<Definition, FileSystem, State> cache2 = HashBasedTable.create();

    private final Definition def;
    private final FileSystem fs;
    
    private State(Definition def, FileSystem fs) {
        this.def = def;
        this.fs = fs;
    }
    
    /**
     * Only used when the Term is part of a Definition instead of part of a
     * ConstrainedTerm.
     */
    public static State of(Definition def) {
        State state = cache1.get(def);
        if (state == null) {
            state = new State(def, null);
            cache1.put(def, state);
        }
        return state;
    }

    public static State of(Definition def, FileSystem fs) {
        assert fs != null;
        State state = cache2.get(def, fs);
        if (state == null) {
            state = new State(def, fs);
            cache2.put(def, fs, state);
        }
        return state;
    }

    public Definition definition() {
        return def;
    }

    public FileSystem fileSystem() {
        return fs;
    }

    @Override
    public ASTNode accept(Transformer transformer) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void accept(Visitor visitor) {
        throw new UnsupportedOperationException();
    }

}
