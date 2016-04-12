// Copyright (c) 2014-2016 K Team. All Rights Reserved.
package org.kframework.backend.java.kil;

import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.collections4.trie.PatriciaTrie;

import com.google.common.collect.ImmutableSet;
import org.kframework.builtin.Sorts;
import org.kframework.definition.ModuleName;
import org.kframework.utils.errorsystem.KEMException;

/**
 * Sort of a {@link Term}.
 *
 * @author YilongL
 *
 */
public final class Sort extends org.kframework.kore.AbstractSort implements MaximalSharing, Serializable {

    private static final ConcurrentMap<String, Sort> cache = new ConcurrentHashMap<>();

    /**
     * see {@link #ordinal()}
     */
    public static final AtomicInteger maxOrdinal = new AtomicInteger(0);


    public static final Sort KITEM          =   Sort.of(Sorts.KItem().name());
    public static final Sort KSEQUENCE      =   Sort.of(Sorts.K().name());
    public static final Sort KLIST          =   Sort.of(Sorts.KList().name());
    public static final Sort KLABEL         =   Sort.of(Sorts.KLabel().name());
    public static final Sort KRESULT        =   Sort.of(Sorts.KResult().name());

    public static final Sort BAG            =   Sort.of("Bag@KCELLS");
    public static final Sort LIST           =   Sort.of("List@LIST");
    public static final Sort MAP            =   Sort.of("Map@MAP");
    public static final Sort SET            =   Sort.of("Set@SET");

    public static final Sort INT            =   Sort.of("Int@INT-SYNTAX");
    public static final Sort BOOL           =   Sort.of("Bool@BOOL-SYNTAX");
    public static final Sort FLOAT          =   Sort.of("Float@FLOAT-SYNTAX");
    public static final Sort STRING         =   Sort.of("String@STRING-SYNTAX");
    public static final Sort BIT_VECTOR     =   Sort.of("MInt@MINT");
    public static final Sort META_VARIABLE  =   Sort.of("MetaVariable@BACKEND_ONLY");

    public static final Sort KVARIABLE      =   Sort.of("KVariable@SUBSTITUTION");

    public static final Sort BOTTOM         =   Sort.of("Bottom@BACKEND_ONLY");
    public static final Sort MGU            =   Sort.of("Mgu@BACKEND_ONLY");

    /**
     * {@code String} representation of this {@code Sort}.
     */
    private final String name;

    private final int ordinal;

    /**
     * Gets the corresponding {@code Sort} from its {@code String}
     * representation.
     *
     * @param name
     *            the name of the sort
     * @return the sort
     */
    public static Sort of(String name) {
        if(!name.contains("@"))
            throw new AssertionError("Backend should only get fully qulified sorts, but got: " + name);
        return cache.computeIfAbsent(name, s -> new Sort(name, maxOrdinal.getAndIncrement()));
    }

    public static Sort of(org.kframework.kil.Sort sort) {
        return of(sort.getName());
    }

    public static Set<Sort> of(Collection<org.kframework.kil.Sort> sorts) {
        ImmutableSet.Builder<Sort> builder = ImmutableSet.builder();
        for (org.kframework.kil.Sort name : sorts) {
            builder.add(Sort.of(name.getName()));
        }
        return builder.build();
    }

    private Sort(String name, int ordinal) {
        this.name = name;
        this.ordinal = ordinal;
    }

    public String name() {
        return name;
    }

    public int ordinal() {
        return ordinal;
    }

    public Sort getUserListSort(String separator) {
        return Sort.of(org.kframework.kil.Sort.LIST_OF_BOTTOM_PREFIX + name
                + "{\"" + separator + "\"}");
    }

    public org.kframework.kil.Sort toFrontEnd() {
        return org.kframework.kil.Sort.of(name);
    }

    @Override
    public boolean equals(Object object) {
        return this == object;
    }

    @Override
    public String toString() {
        return name;
    }

    /**
     * Returns the cached instance rather than the de-serialized instance if
     * there is a cached instance.
     */
    Object readResolve() throws ObjectStreamException {
        if (cache.containsKey(name) && cache.get(name).ordinal != this.ordinal) {
            KEMException.criticalError("The ordinal for sort: " + name + " is " + cache.get(name).ordinal +
                    " in the cache and " + this.ordinal + " serialized.");
        }
        // TODO: fix bug: ordinals from deserialized objects may overlap with those of newly created objects
        return cache.computeIfAbsent(name, x -> this);
    }

    @Override
    public ModuleName moduleName() {
        return ModuleName.STAR();
    }

    @Override
    public String localName() {
        return name;
    }
}
