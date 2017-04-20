// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.builtin;

import org.kframework.legacykore.K;
import org.kframework.legacykore.KApply;
import org.kframework.legacykore.KToken;

import static org.kframework.legacykore.KORE.KApply;
import static org.kframework.legacykore.KORE.KLabel;
import static org.kframework.legacykore.KORE.KToken;
import static org.kframework.legacykore.KORE.Sort;

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
