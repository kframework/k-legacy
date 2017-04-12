// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.legacykore;

import org.kframework.kore.AbstractFoldK;

/**
 * Checks whether particular K pattern given as a visitor exists.
 */
public class ExistsK extends AbstractFoldK<Boolean> {
    @Override
    public Boolean unit() {
        return false;
    }

    @Override
    public Boolean merge(Boolean a, Boolean b) {
        return a || b;
    }
}
