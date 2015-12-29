package org.kframework.kore.kdoc;

import org.kframework.kompile.CompiledDefinition;
import org.kframework.kompile.ConcreteDefinition;

/**
 * Created by Edgar Pek on 9/14/15.
 */
public abstract class PosterBackend {
    public abstract void run(CompiledDefinition def);
}
