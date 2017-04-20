// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.legacykore.compile;

import org.kframework.definition.Definition;
import org.kframework.kompile.CompiledDefinition;

import java.util.function.Function;

/**
 * Created by dwightguth on 9/1/15.
 */
public interface Backend {

    void accept(CompiledDefinition def);

    Function<Definition, Definition> steps();
}
