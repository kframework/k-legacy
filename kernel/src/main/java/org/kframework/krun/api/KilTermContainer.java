// Copyright (c) 2014-2015 K Team. All Rights Reserved.
package org.kframework.krun.api;

import org.kframework.kil.Term;
import org.kframework.kil.loader.Context;

import java.util.Optional;

/**
 * Abstract class, to be extended by every container specific to the backend.
 * Allows for lazy evaluation of a backend specific term to a generic kernel term.
 */
public abstract class KilTermContainer {

    protected Optional<Term> kilTerm;

    protected Context context;

    public KilTermContainer(Context context) {
        this.context = context;
        this.kilTerm = Optional.ofNullable(null);
    }

    public KilTermContainer(Context context, Term kilTerm) {
        this.kilTerm = Optional.of(kilTerm);
        this.context = context;
    }

    /**
     * To be implemented in the extending class.
     * When called, the function should return the kilTerm, if already calculated,
     * or calculate it using the context and the backend term, and then return it.
     */

    public abstract Term getKilTerm();

    public abstract boolean equals(Object obj);

    public abstract int hashCode();
}
