package org.kframework.kompile;

import org.kframework.kore.Definition;

import java.io.Serializable;

/**
 * {@link org.kframework.kore.Definition} does not implement serializable.
 * Hence, we wrap it in a serializable class.
 * Serializability occurs along with textual kore representation, but allows
 * for performance gains during KRun.
 */
public class SerializableKoreDefinition implements Serializable {
    private org.kframework.kore.Definition definition;

    public SerializableKoreDefinition(Definition definition) {
        this.definition = definition;
    }

    public Definition getDefinition() {
        return definition;
    }
}
