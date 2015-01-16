// Copyright (c) 2014-2015 K Team. All Rights Reserved.
package org.kframework.backend.java.util;

import org.kframework.backend.java.symbolic.BackendJavaKILtoKILTransformer;
import org.kframework.kil.loader.Context;
import org.kframework.kil.Rule;
import org.kframework.krun.api.KilRuleContainer;

/**
 * Java Backend Specific Subclass of the Generic
 * Kil Rule Container.
 */
public class JavaKilRuleContainer extends KilRuleContainer{

    private org.kframework.backend.java.kil.Rule javaRule;

    public JavaKilRuleContainer(org.kframework.backend.java.kil.Rule javaRule, Context context) {
        super(context);
        this.javaRule = javaRule;
    }

    public org.kframework.backend.java.kil.Rule getJavaRule() {
        return javaRule;
    }

    /**
     * Lazily evaluates the Generic Kil Rule from java backend rule.
     * @return Rule: Generic kil rule.
     */
    @Override
    public Rule getKilTerm() {
        if(rule.isPresent()) {
            return rule.get();
        }
        return  (Rule) javaRule.accept(
                new BackendJavaKILtoKILTransformer(context)
        );
    }

    @Override
    public boolean equals(Object obj) {
        if(! (obj instanceof JavaKilRuleContainer)) {
            return false;
        }
        JavaKilRuleContainer javaRuleContainer = (JavaKilRuleContainer) obj;
        return javaRuleContainer.getJavaRule().equals(javaRule);
    }

    @Override
    public int hashCode() {
        return javaRule.hashCode();
    }
}
