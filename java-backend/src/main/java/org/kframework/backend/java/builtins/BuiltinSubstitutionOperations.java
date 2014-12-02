// Copyright (c) 2014 K Team. All Rights Reserved.
package org.kframework.backend.java.builtins;


import org.kframework.backend.java.kil.*;
import org.kframework.backend.java.symbolic.UnboundedTermsCollector;
import org.kframework.backend.java.symbolic.UserSubstitutionTransformer;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Table of {@code public static} methods on builtin user-level substitution.
 *
 * @author: TraianSF
 */
public class BuiltinSubstitutionOperations {

    public static Term userSubstitution(Term term, Term substitute, Term variable, TermContext context) {
        Map<Term, Term> substitution = new HashMap<>();
        substitution.put(variable, substitute);
        return KLabelInjection.injectionOf(UserSubstitutionTransformer.userSubstitution(substitution, term, context), context);
    }

    public static Term freeVariables(Term term, TermContext context) {
        Set<Term> terms = UnboundedTermsCollector.getUnboundedTerms(term);
        BuiltinSet.Builder builder = BuiltinSet.builder();
        for (Term unboundedTerm : terms) {
            if (unboundedTerm instanceof Variable) {
                builder.add(unboundedTerm);
            }
        }
        return builder.build();
    }

}
