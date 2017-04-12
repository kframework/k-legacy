// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.legacykore;

import org.kframework.kore.AbstractKTransformer;
import org.kframework.kore.InjectedKLabel;
import org.kframework.kore.K;
import org.kframework.kore.KApply;
import org.kframework.kore.KLabel;
import org.kframework.kore.KORE;
import org.kframework.kore.KRewrite;
import org.kframework.kore.KSequence;
import org.kframework.kore.KToken;
import org.kframework.kore.KVariable;

import java.util.ArrayList;

import static org.kframework.kore.KORE.*;

/**
 * Abstract K to K transformer.
 */
public class TransformK extends AbstractKTransformer<K> {

    @Override
    public K apply(KApply k) {
        ArrayList<K> newItems = new ArrayList<>(k.klist().items());
        boolean change = false;
        for (int i = 0; i < newItems.size(); ++i) {
            K in = newItems.get(i);
            K out = apply(in);
            newItems.set(i, out);
            change = change || (in != out);
        }
        if (change) {
            return KORE.KApply(apply(k.klabel()), KList(newItems), k.att());
        } else {
            return k;
        }
    }

    private KLabel apply(KLabel klabel) {
        return klabel;
    }

    @Override
    public K apply(KRewrite k) {
        K l = apply(k.left());
        K r = apply(k.right());
        if (l != k.left() || r != k.right()) {
            return KRewrite(l, r, k.att());
        } else {
            return k;
        }
    }

    @Override
    public K apply(KToken k) {
        return k;
    }

    @Override
    public K apply(KVariable k) {
        return k;
    }

    @Override
    public K apply(KSequence k) {
        ArrayList<K> newItems = new ArrayList<>(k.items());
        boolean change = false;
        for (int i = 0; i < newItems.size(); ++i) {
            K in = newItems.get(i);
            K out = apply(newItems.get(i));
            newItems.set(i, out);
            change = change || (in != out);
        }
        if (change) {
            return KSequence(newItems, k.att());
        } else {
            return k;
        }
    }

    @Override
    public K apply(InjectedKLabel k) {
        return k;
    }
}
