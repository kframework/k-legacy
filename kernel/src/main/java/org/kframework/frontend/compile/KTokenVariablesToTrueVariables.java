// Copyright (c) 2016 K Team. All Rights Reserved.

package org.kframework.frontend.compile;

import org.kframework.attributes.Att;
import org.kframework.definition.Module;
import org.kframework.frontend.K;
import org.kframework.frontend.KToken;
import org.kframework.frontend.SortedADT;
import org.kframework.frontend.TransformK;
import scala.Option;

import java.util.function.BiFunction;

/**
 * Used by logik to down Variables represented as tokens to true Variables
 * Should be removed once we have true meta-level.
 */
public class KTokenVariablesToTrueVariables implements BiFunction<Module, K, K> {
    @Override
    public K apply(Module module, K k) {
        return new TransformK() {
            public K apply(KToken t) {
                Option<Att> attOption = module.attForSort().get(t.sort());
                if(attOption.isDefined() && attOption.get().contains(Att.variable())) {
                    return new SortedADT.SortedKVariable(t.s(), t.att().add(Att.sort(), t.sort()));
                } else {
                    return t;
                }
            }
        }.apply(k);
    }
}
