// Copyright (c) 2014-2015 K Team. All Rights Reserved.
package org.kframework.krun.api;

import org.kframework.kil.Term;
import org.kframework.kil.loader.Context;


/**
 * Generic Wrapper Class for Terms that have already been converted
 * to Generic Terms.
 */
public class GenericKilTermContainer extends KilTermContainer{

    public GenericKilTermContainer(Context context, Term term) {
        super(context, term);
    }

    @Override
    public Term getKilTerm() {
        return kilTerm.get();
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof GenericKilTermContainer)) {
            return false;
        }
        GenericKilTermContainer genericTerm = (GenericKilTermContainer) obj;
        return SemanticEqual.checkEquality(genericTerm.getKilTerm(), this.getKilTerm());
    }

    @Override
    public int hashCode() {
        return kilTerm.get().hashCode();
    }
}
