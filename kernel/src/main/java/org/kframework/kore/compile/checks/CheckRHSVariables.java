// Copyright (c) 2015 K Team. All Rights Reserved.
package org.kframework.kore.compile.checks;

import com.google.common.collect.Sets;
import org.kframework.definition.Context;
import org.kframework.definition.Rule;
import org.kframework.definition.Sentence;
import org.kframework.kore.FoldKIntoSet;
import org.kframework.kore.InjectedKLabel;
import org.kframework.kore.K;
import org.kframework.kore.KApply;
import org.kframework.kore.KRewrite;
import org.kframework.kore.KVariable;
import org.kframework.kore.compile.ResolveAnonVar;
import org.kframework.kore.compile.RewriteAwareVisitor;
import org.kframework.utils.errorsystem.KEMException;
import scala.collection.Set;

import java.util.stream.Stream;

import static org.kframework.Collections.*;

/**
 * Checks a sentence to determine if it declares any variables in the RHS that are not bound in the LHS.
 * <p>
 * More specifically, it performs the following checks: for each anonymous variable, if it is on the right hand side
 * of a rewrite operator, and does not signify a fresh variable or constant, it is an error. For each non-anonymous
 * variable, if it is on the right hand side of a rewrite operator, it is an error if it does not appear anywhere on the
 * left hand side of the rule, and does not signify a fresh variable or constant.
 */
public class CheckRHSVariables {
    private final java.util.Set<KEMException> errors;

    public CheckRHSVariables(java.util.Set<KEMException> errors) {
        this.errors = errors;
    }

    private void check(Rule rule) {
        if (rule.att().contains("unblock"))
            return;
        resetVars();
        gatherVars(rule.body());
        gatherVars(rule.requires());
        gatherVars(rule.ensures());
        check(rule.body());
        check(rule.requires());
        check(rule.ensures());
    }

    private void check(Context context) {
        resetVars();
        gatherVars(context.body());
        gatherVars(context.requires());
        check(context.body());
        check(context.requires());
    }

    private void shortCheck(K k) {
        Set<KRewrite> rewrites = new FoldKIntoSet<KRewrite>() {
            public Set<KRewrite> apply(KRewrite rw) {
                return Set(rw);
            }
        }.apply(k);
        FoldKIntoSet<KVariable> gatherVars = new FoldKIntoSet<KVariable>() {
            public Set<KVariable> apply(KVariable v) {
                return Set(v);
            }
        };
        Set<KVariable> leftVars = stream(rewrites).flatMap(rw -> stream(gatherVars.apply(rw.left()))).collect(toSet());
        Stream<KVariable> rightVarsStream = stream(rewrites).flatMap(rw -> stream(gatherVars.apply(rw.right())));

        rightVarsStream.forEach(v -> {
            if (v.equals(ResolveAnonVar.ANON_VAR)
                    || (!v.equals(ResolveAnonVar.ANON_VAR) && !(v.name().startsWith("?") || v.name().startsWith("!")) && !vars.contains(v))) {
                errors.add(KEMException.compilerError("Found variable " + v.name()
                        + " on right hand side of rule, not bound on left hand side."
                        + " Did you mean \"?" + v.name() + "\"?", v));
            }
        });
    }

    public void check(Sentence s) {
        if (s instanceof Rule) {
            check((Rule) s);
        } else if (s instanceof Context) {
            check((Context) s);
        }
    }

    private java.util.Set<KVariable> vars = Sets.newHashSet();

    void resetVars() {
        vars.clear();
    }

    void gatherVars(K term) {
        new RewriteAwareVisitor() {
            @Override
            public void apply(KVariable v) {
                if (isLHS() && !v.equals(ResolveAnonVar.ANON_VAR))
                    vars.add(v);
                super.apply(v);
            }

            @Override
            public void apply(KApply k) {
                if (k.klabel() instanceof KVariable) {
                    apply((KVariable) k.klabel());
                }
                super.apply(k);
            }

            @Override
            public void apply(InjectedKLabel k) {
                if (k.klabel() instanceof KVariable) {
                    apply((KVariable) k.klabel());
                }
                super.apply(k);
            }
        }.apply(term);
    }

    private void check(K body) {
        new RewriteAwareVisitor() {
            @Override
            public void apply(KVariable k) {
                if (isRHS()) {
                    if ((k.equals(ResolveAnonVar.ANON_VAR) && !isLHS())
                            || (!k.equals(ResolveAnonVar.ANON_VAR) && !(k.name().startsWith("?") || k.name().startsWith("!")) && !vars.contains(k))) {
                        errors.add(KEMException.compilerError("Found variable " + k.name()
                                + " on right hand side of rule, not bound on left hand side."
                                + " Did you mean \"?" + k.name() + "\"?", k));
                    }
                }
            }

            @Override
            public void apply(InjectedKLabel k) {
                if (k.klabel() instanceof KVariable) {
                    apply((KVariable) k.klabel());
                }
                super.apply(k);
            }

            @Override
            public void apply(KApply k) {
                if (k.klabel() instanceof KVariable) {
                    apply((KVariable) k.klabel());
                }
                super.apply(k);
            }
        }.apply(body);
    }
}
