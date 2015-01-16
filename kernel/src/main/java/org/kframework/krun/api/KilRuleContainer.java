// Copyright (c) 2014-2015 K Team. All Rights Reserved.
package org.kframework.krun.api;

import org.kframework.kil.loader.Context;
import org.kframework.kil.Rule;

import java.util.Optional;

/**
 * Container for Generic Kil Rules.
 */
public abstract class KilRuleContainer {
    protected Optional<Rule> rule;

    protected Context context;

    public KilRuleContainer(Rule rule, Context context) {
        this.rule = Optional.ofNullable(rule);
        this.context = context;
    }

    public KilRuleContainer(Context context) {
        this.context = context;
        rule = Optional.empty();
    }

    /**
     * To be implemented in the extending class.
     * When called, the function should return the kilRule, if already calculated,
     * or calculate it using the context and the backend Rule, and then return it.
     */

    public abstract Rule getKilTerm();

    public abstract boolean equals(Object obj);

    public abstract int hashCode();
}
