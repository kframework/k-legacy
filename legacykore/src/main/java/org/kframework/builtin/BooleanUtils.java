// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.builtin;

import org.kframework.frontend.K;
import org.kframework.frontend.KApply;
import org.kframework.frontend.KToken;

import static org.kframework.frontend.KORE.KApply;
import static org.kframework.frontend.KORE.KLabel;
import static org.kframework.frontend.KORE.KToken;
import static org.kframework.frontend.KORE.Sort;

/**
 * Created by dwightguth on 4/17/15.
 */
public class BooleanUtils {

    public static KApply and(K k1, K k2) {
        return KApply(KLabel("_andBool_"), k1, k2);
    }
    public static KApply not(K k) { return KApply(KLabel("notBool_"), k); }

    public static final KToken TRUE = KToken("true", Sorts.Bool());
    public static final KToken FALSE = KToken("false", Sorts.Bool());
}
