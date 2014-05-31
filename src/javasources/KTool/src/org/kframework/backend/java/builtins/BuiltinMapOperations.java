// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.backend.java.builtins;

import org.kframework.backend.java.kil.BuiltinMap;
import org.kframework.backend.java.kil.BuiltinSet;
import org.kframework.backend.java.kil.Term;
import org.kframework.backend.java.kil.TermContext;


/**
 * Table of {@code public static} methods on builtin maps.
 *
 * @author AndreiS
 */
public class BuiltinMapOperations {

    public static BuiltinSet keys(BuiltinMap map, TermContext context) {
        if (!map.hasFrame()) {
            return new BuiltinSet(map.keySet());
        } else {
            throw new IllegalArgumentException("argument " + map + " has frame");
        }
    }

    public static BuiltinMap construct(BuiltinMap term1, BuiltinMap term2, TermContext context) {
        return term1.combine(term2);
    }

    public static BuiltinMap update(BuiltinMap map, Term key, Term value, TermContext context) {
        // TODO(YilongL): is this method still being used? why is it different
        // from {@link MapUpdate} which allows adding new keys to map with
        // frame?
        if (map.hasFrame()) {
            throw new IllegalArgumentException("MapBuiltin " + map + " has frame");
        } else {
            return map.assign(key, value);
        }
    }

}
