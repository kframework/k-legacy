// Copyright (c) 2014-2015 K Team. All Rights Reserved.
package org.kframework.backend.java.util;

import org.kframework.backend.java.symbolic.BackendJavaKILtoKILTransformer;
import org.kframework.kil.loader.Context;
import org.kframework.kil.Term;
import org.kframework.krun.api.KilTermContainer;

/**
 * The java backend specific class for the lazy evaluating container.
 * Implements the converter that lazily converts the backend kilTerm to the generic term.
 */
public class JavaKilTermContainer extends KilTermContainer {

    private org.kframework.backend.java.kil.Term javaTerm;

    public org.kframework.backend.java.kil.Term getJavaTerm() {
        return javaTerm;
    }

    public JavaKilTermContainer(Context context, org.kframework.backend.java.kil.Term javaTerm) {
        super(context);
        this.javaTerm = javaTerm;
    }


    @Override
    public Term getKilTerm() {
        //Todo: Replace this with Java 8's functional features
        if(kilTerm.isPresent()) {
            return kilTerm.get();
        }
        return (Term) javaTerm.accept(
                new BackendJavaKILtoKILTransformer(context)
        );
    }

    @Override
    public boolean equals(Object KilBuffer2) {
        if (! (KilBuffer2 instanceof JavaKilTermContainer)) {
            return false;
        }
        return this.javaTerm.equals(((JavaKilTermContainer) KilBuffer2).getJavaTerm());
    }

    @Override
    public int hashCode() {
        return javaTerm.hashCode();
    }
}
