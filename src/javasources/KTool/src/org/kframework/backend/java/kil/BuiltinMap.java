// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.backend.java.kil;

import immutablecollections.ImMap;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.AbstractMap;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.kframework.backend.java.symbolic.KILtoBackendJavaKILTransformer;
import org.kframework.backend.java.symbolic.Matcher;
import org.kframework.backend.java.symbolic.Transformer;
import org.kframework.backend.java.symbolic.Unifier;
import org.kframework.backend.java.symbolic.Visitor;
import org.kframework.backend.java.util.KSorts;
import org.kframework.backend.java.util.Utils;
import org.kframework.kil.ASTNode;
import org.kframework.kil.DataStructureSort;

import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableSet;


/**
 * Class representing a map. It only has one frame (which is a variable), and a set of entries.
 * A map composed of multiple map variables or terms (in addition to the entries) is represented
 * using concatenation (and can only occur in the right-hand side or in the condition).
 *
 * @author AndreiS
 */
public class BuiltinMap extends Collection implements Iterable<Map.Entry<Term, Term>> {

    public static final BuiltinMap EMPTY_MAP = new BuiltinMap();

    /**
     * The underlying implementation of the persistent hash map. Marked as
     * transient because its serialzation is not properly implemented.
     * 
     * @see {@link #readObject(ObjectInputStream)} and
     *      {@link #writeObject(ObjectOutputStream)}
     */
    private transient final ImMap<Term, Term> entries;

    private ImmutableSet<Term> keys = null;

    public BuiltinMap(BuiltinMap map) {
        super(map.frame, Kind.KITEM);
        this.entries = map.entries;
        this.keys = map.keys;
    }

    private BuiltinMap() {
        super(null, Kind.KITEM);
        entries = ImMap.empty();
        keys = ImmutableSet.<Term>builder().build();
    }

    /**
     * Efficient private constructor for immutable BuiltinMap.
     */
    private BuiltinMap(ImMap<Term, Term> entries, Variable frame) {
        super(frame, Kind.KITEM);
        this.entries = entries;
    }

    /**
     * Creates a new {@code BuiltinMap} by combining a builtin map with another
     * term.
     * 
     * @param map
     *            the builtin map
     * @param term
     *            the term
     * @return the new builtin map
     */
    public static BuiltinMap of(BuiltinMap map, Term term) {
        if (term == null) {
            return map;
        }
        if (term instanceof Variable) {
            if (map.hasFrame()) {
                throw new IllegalArgumentException(
                        "BuiltinMap " + map + " already has a frame variable; unable to combine it with another one.");
            }
            return new BuiltinMap(map.entries, (Variable) term);
        }
        if (term instanceof BuiltinMap) {
            return map.combine((BuiltinMap) term);
        }
        assert false : "Frame can only be substituted by a Variable or a BuiltinMap, or deleted.";
        return null;
    }

    /**
     * Creates a new {@code BuiltinMap} from a given {@code Map} instance and a
     * frame variable. All entries of the given map must be copied to build the
     * new persistent map.
     * <p>
     * This method was intended to be used iby
     * {@link KILtoBackendJavaKILTransformer} only.
     * 
     * @param map
     * @param frame
     * @return the new builtin map
     */
    public static BuiltinMap makePersistentMap(Map<Term, Term> map, Variable frame) {
        ImMap<Term, Term> entries  = ImMap.empty();
        for (Map.Entry<Term, Term> entry : map.entrySet()) {
            entries = entries.put(entry.getKey(), entry.getValue());
        }
        return new BuiltinMap(entries, frame);
    }

    /**
     * Creates a new {@code BuiltinMap} from a given {@code Map} instance. All
     * entries of the given map must be copied to build the new persistent map.
     * <p>
     * This method was intended to be used by
     * {@link KILtoBackendJavaKILTransformer} only.
     * 
     * @param map
     * @param frame
     * @return the new builtin map
     */
    public static BuiltinMap makePersistentMap(Map<Term, Term> map) {
        return makePersistentMap(map, null);
    }
    
    private class BuiltinMapIterator implements Iterator<Map.Entry<Term, Term>> {

        Iterator<ImMap.Entry<Term, Term>> iterator = BuiltinMap.this.entries.iterator();

        @Override
        public boolean hasNext() {
            return iterator.hasNext();
        }

        @Override
        public Map.Entry<Term, Term> next() {
            ImMap.Entry<Term, Term> entry = iterator.next();
            return new AbstractMap.SimpleEntry<Term, Term>(entry.getKey(), entry.getValue());
        }

        @Override
        public void remove() {
            iterator.remove();
        }

    }

    @Override
    public Iterator<Map.Entry<Term, Term>> iterator() {
        return new BuiltinMapIterator();
    }

    /**
     * Creates a new {@code BuiltinMap} by combining this builtin map with
     * another given builtin map.
     * 
     * @param otherMapBuiltin
     * @return the new builtin map
     */
    public BuiltinMap combine(BuiltinMap otherMapBuiltin) {
        Variable frame = null;
        if (this.hasFrame() && otherMapBuiltin.hasFrame()) {
            throw new IllegalArgumentException(
                    "both MapBuiltins have frames, but the combined map cannot have two frames");
        } else if (otherMapBuiltin.hasFrame()) {
            frame = this.frame;
        } else {
            frame = otherMapBuiltin.frame;
        }

        /* use the larger map as the initial base */
        ImMap<Term, Term> base = this.entries.size() > otherMapBuiltin.entries.size() ?
                this.entries : otherMapBuiltin.entries;
        ImMap<Term, Term> src = base == this.entries ? otherMapBuiltin.entries : this.entries;
        for (ImMap.Entry<Term, Term> e : src) {
            base = base.put(e.getKey(), e.getValue());
        }
        return new BuiltinMap(base, frame);
    }
    
    /**
     * Returns the value associated with the given key.
     * 
     * @param key
     * @return the value
     */
    public Term get(Term key) {
        return entries.get(key);
    }

    /**
     * Creates a new {@code BuiltinMap} by putting the given key-value pair.
     * @param key
     * @param value
     * @return the new builtin map
     */
    public BuiltinMap assign(Term key, Term value) {
        return new BuiltinMap(entries.put(key, value), frame);
    }

    /**
     * Creates a {@code BuiltinMap} by putting all entries of a given map.
     * 
     * @param map
     * @return the new builtin map
     */
    public BuiltinMap assignAll(Map<Term, Term> map) {
        BuiltinMap result = this;
        for (Map.Entry<Term, Term> e : map.entrySet()) {
            result = result.assign(e.getKey(), e.getValue());
        }
        return result;
    }

    /**
     * Creates a new {@code BuiltinMap} by deleting the given key.
     * 
     * @param key
     *            the key to delete
     * @return the new builtin map
     */
    public BuiltinMap delete(Term key) {
        return new BuiltinMap(entries.remove(key), frame);
    }

    /**
     * Creates a new {@code BuiltinMap} by replacing its frame with the given
     * term.
     * 
     * @param replacement
     *            the new term to replace it with
     * @return the new builtin map
     */
    public BuiltinMap replaceFrame(Term replacement) {
        if (frame == replacement) {
            return this;
        }
        
        if (replacement == null) {
            return new BuiltinMap(this.entries, null);
        }
        if (replacement instanceof Variable) {
            return new BuiltinMap(this.entries, (Variable) replacement);
        }
        if (replacement instanceof BuiltinMap) {
            return new BuiltinMap(this.entries, null).combine((BuiltinMap) replacement);
        }
        assert false : "Frame can only be replaced by a Variable or a BuiltinMap, or deleted.";
        return null;
    }

    /**
     * Returns a set of all keys in the entries of this {@code BuiltinMap}.
     * 
     * @return the key set
     */
    public Set<Term> keySet() {
        // YilongL: this operation is rarely used; so it's okay to let it be
        // slow and, thus, make put/remove as efficient as possible
        if (keys != null) {
            return keys;
        }

        ImmutableSet.Builder<Term> builder = ImmutableSet.builder();
        for (ImMap.Entry<Term, Term> e : this.entries) {
            builder.add(e.getKey());
        }
        keys = builder.build();
        return keys;
    }

    @Override
    public int size() {
        return entries.size();
    }

    @Override
    public boolean isExactSort() {
        return true;
    }

    @Override
    public String sort() {
        // TODO(AndreiS): track the original sort from the grammar
        return KSorts.MAP;
    }

    @Override
    public boolean equals(Object object) {
        if (this == object) {
            return true;
        }

        if (!(object instanceof BuiltinMap)) {
            return false;
        }

        BuiltinMap map = (BuiltinMap) object;
        return (frame == null ? map.frame == null : frame.equals(map.frame))
                && entries.equals(map.entries);
    }

    @Override
    public int computeHash() {
        int hashCode = 1;
        hashCode = hashCode * Utils.HASH_PRIME + (frame == null ? 0 : frame.hashCode());
        hashCode = hashCode * Utils.HASH_PRIME + entries.hashCode();
        return hashCode;
    }

    @Override
    public String toString() {
        return toString(" ", " |-> ", DataStructureSort.DEFAULT_MAP_UNIT_LABEL);
    }

    private String toString(String operator, String mapsTo, String identity) {
        Joiner.MapJoiner joiner = Joiner.on(operator).withKeyValueSeparator(mapsTo);
        StringBuilder stringBuilder = new StringBuilder();
        joiner.appendTo(stringBuilder, this.iterator());
        if (super.hasFrame()) {
            if (stringBuilder.length() != 0) {
                stringBuilder.append(operator);
            }
            stringBuilder.append(super.frame());
        }
        if (stringBuilder.length() == 0) {
            stringBuilder.append(identity);
        }
        return stringBuilder.toString();
    }

    @Override
    public void accept(Unifier unifier, Term pattern) {
        unifier.unify(this, pattern);
    }

    @Override
    public void accept(Matcher matcher, Term pattern) {
        matcher.match(this, pattern);
    }

    @Override
    public void accept(Visitor visitor) {
        visitor.visit(this);
    }

    @Override
    public ASTNode accept(Transformer transformer) {
        return transformer.transform(this);
    }
    
    /**
     * Serializes the {@link ImMap} as a sequence of key-value pairs because it
     * doesn't properly implement {@link Serializable} as {@link HashMap}, which
     * would rebuild the {@code HashMap} during deserialization to cope with
     * changing @{@code hashCode}. In particular, {@link Variable#readResolve()}
     * renames anonymous variable during deserialization to avoid name clashes.
     * 
     * @param out
     *            the output object stream
     * @throws IOException
     */
    private void writeObject(ObjectOutputStream out) throws IOException {
        out.defaultWriteObject();
        out.writeObject(frame);
        out.writeObject(keys);
        out.writeInt(entries.size());
        for (Map.Entry<Term, Term> e : this) {
            out.writeObject(e.getKey());
            out.writeObject(e.getValue());
        }
    }
    
    /**
     * Deserializes this {@code BuiltinMap} object. In particular, it rebuilds
     * the {@code entries} during deserialization.
     * 
     * @param in
     *            the input object stream
     * @throws IOException
     * @throws ClassNotFoundException
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     * @throws NoSuchFieldException
     * @throws SecurityException
     */
    @SuppressWarnings("unchecked")
    private void readObject(ObjectInputStream in) throws IOException,
            ClassNotFoundException, IllegalArgumentException,
            IllegalAccessException, NoSuchFieldException, SecurityException {
        in.defaultReadObject();
        Field f = Collection.class.getDeclaredField("frame");
        f.setAccessible(true);
        f.set(this, in.readObject());
        
        keys = (ImmutableSet<Term>) in.readObject();
        
        ImMap<Term, Term> imMap = ImMap.empty();
        int size = in.readInt();
        for (int i = 0; i < size; i++) {
            Term key = (Term) in.readObject();
            Term value = (Term) in.readObject();
            imMap = imMap.put(key, value);
        }
        f = BuiltinMap.class.getDeclaredField("entries");
        f.setAccessible(true);
        f.set(this, imMap);
    }
}
