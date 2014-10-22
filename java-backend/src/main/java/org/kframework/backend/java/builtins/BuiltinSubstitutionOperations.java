// Copyright (c) 2014 K Team. All Rights Reserved.
package org.kframework.backend.java.builtins;


import org.kframework.backend.java.kil.*;
import org.kframework.backend.java.kil.KItem.KItemOperations;
import org.kframework.backend.java.symbolic.UserSubstitutionTransformer;

import com.google.inject.Inject;

import java.util.HashMap;
import java.util.Map;

/**
 * Table of {@code public static} methods on builtin user-level substitution.
 *
 * @author: TraianSF
 */
public class BuiltinSubstitutionOperations {

    private final KItemOperations kItemOps;

    @Inject
    public BuiltinSubstitutionOperations(KItemOperations kItemOps) {
        this.kItemOps = kItemOps;
    }

    public Term userSubstitution(Term term, Term substitute, Term variable, TermContext context) {
        Map<Term, Term> substitution = new HashMap<>();
        substitution.put(variable, substitute);
        return kItemOps.injectionOf(UserSubstitutionTransformer.userSubstitution(substitution, term, context), context);
    }

}
