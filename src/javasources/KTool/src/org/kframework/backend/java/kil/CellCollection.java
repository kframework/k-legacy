// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.backend.java.kil;

import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.kframework.backend.java.symbolic.Matcher;
import org.kframework.backend.java.symbolic.Transformer;
import org.kframework.backend.java.symbolic.Unifier;
import org.kframework.backend.java.symbolic.Visitor;
import org.kframework.backend.java.util.Utils;
import org.kframework.kil.ASTNode;
import org.kframework.kil.DataStructureSort;
import org.kframework.kil.DataStructureSort.Label;
import org.kframework.kil.loader.Context;

import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableMultiset;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Multiset;


/**
 * Represents a collection of {@link Cell}s. The ordering of the internal cells
 * is fixed and agrees with the ordering of the cells used to construct this
 * cell collection.
 *
 * @author AndreiS
 *
 */
@SuppressWarnings("rawtypes")
public class CellCollection extends Collection {

    public static final CellCollection EMPTY = new CellCollection(
            ImmutableListMultimap.<String, Cell> of(),
            ImmutableMultiset.<Variable> of(),
            false);

    /**
     * Choose {@code ListMultimap} over {@code SetMultimap} because we need to
     * be able to store identical cells.
     */
    private final ImmutableListMultimap<String, Cell> cells;

    private final ImmutableMultiset<Variable> collectionVariables;

    /**
     * Contains {@code true} if the explicitly specified part of this cell
     * collection contains one or more types of cells whose multiplicity
     * attributes are {@code "*"}'s; otherwise, {@code false}.
     */
    // TODO(AndreiS): handle multiplicity='+'
    private final boolean hasStar;

    public static CellCollection singleton(Cell cell, Context context) {
        return new CellCollection(
                ImmutableListMultimap.of(cell.getLabel(), cell),
                ImmutableMultiset.<Variable> of(),
                context);
    }

    public static Term concatenate(Context context, Term... terms) {
        Builder builder = builder(context);
        for (Term term : terms) {
            builder.concatenate(term);
        }
        return builder.build();
    }

    /**
     * Builds a new {@code CellCollection} by removing all the given cell
     * labels.
     */
    public Term removeAll(Set<String> labelsToRemove, Context context) {
        Builder builder = builder(context);
        for (String label : labelSet()) {
            if (!labelsToRemove.contains(label)) {
                builder.putAll(label, this.get(label));
            }
        }
        if (hasFrame()) {
            builder.concatenate(frame);
        }
        return builder.build();
    }

    /**
     * Retrieves the cell map from a given term representing cell collection.
     */
    public static ListMultimap<String, Cell> getCellMap(Term cellCollection) {
        ListMultimap<String, Cell> cellMap;
        if (cellCollection instanceof Cell) {
            cellMap = ImmutableListMultimap.of(((Cell) cellCollection).getLabel(), (Cell) cellCollection);
        } else if (cellCollection instanceof Variable) {
            cellMap = ImmutableListMultimap.of();
        } else if (cellCollection instanceof CellCollection){
            cellMap = ((CellCollection) cellCollection).cellMap();
        } else {
            assert false : "unexpected argument " + cellCollection;
            cellMap = null;
        }
        return cellMap;
    }

    /**
     * Retrieves the frame variable from a given term representing cell collection.
     */
    public static Variable getFrame(Term cellCollection) {
        Variable frame;
        if (cellCollection instanceof Cell) {
            frame = null;
        } else if (cellCollection instanceof Variable) {
            frame = (Variable) cellCollection;
        } else if (cellCollection instanceof CellCollection){
            frame = ((CellCollection) cellCollection).frame;
        } else {
            assert false : "unexpected argument " + cellCollection;
            frame = null;
        }
        return frame;
    }

    private CellCollection(ImmutableListMultimap<String, Cell> cells,
            ImmutableMultiset<Variable> collectionVariables,
            Context context) {
        this(cells, collectionVariables, numOfMultiplicityCellLabels(cells, context) > 0);
    }

    private CellCollection(ImmutableListMultimap<String, Cell> cells,
            ImmutableMultiset<Variable> collectionVariables,
            boolean hasStar) {
        super(computeFrame(collectionVariables), Kind.CELL_COLLECTION);
        this.cells = cells;
        this.collectionVariables = collectionVariables;
        this.hasStar = hasStar;
    }

    private static Variable computeFrame(ImmutableMultiset<Variable> collectionVariables) {
        return (collectionVariables.size() == 1) ? collectionVariables.iterator().next() : null;
    }

    private static int numOfMultiplicityCellLabels(ListMultimap<String, Cell> cells, Context context) {
        int count = 0;
        for (String cellLabel : cells.keySet()) {
            if (context.getConfigurationStructureMap().get(cellLabel).isStarOrPlus()) {
                count++;
            } else {
                assert cells.get(cellLabel).size() == 1:
                        "cell label " + cellLabel + " does not have multiplicity='*', "
                        + "but multiple cells found " + cells;
            }
        }

        assert count <= 1 :
            "Multiple types of starred cells in one cell collection not supported at present";
        return count;
    }

    public java.util.Collection<Cell> cells() {
        return cells.values();
    }

    public ListMultimap<String, Cell> cellMap() {
        return cells;
    }

    public Multiset<Term> baseTerms() {
        return ImmutableMultiset.<Term>copyOf(collectionVariables);
    }

    public Multiset<Variable> collectionVariables() {
        return collectionVariables;
    }

    public boolean containsKey(String label) {
        return cells.containsKey(label);
    }

    public java.util.Collection<Cell> get(String label) {
        return cells.get(label);
    }

    /**
     * Checks if the explicitly specified part of this cell collection contains
     * one or more types of cells whose multiplicity attributes are {@code "*"}
     * 's.
     */
    public boolean hasStar() {
        return hasStar;
    }

    public Set<String> labelSet() {
        return cells.keySet();
    }

    @Override
    public boolean isEmpty() {
        return cells.isEmpty() && isConcreteCollection();
    }

    @Override
    public int concreteSize() {
        return cells.size();
    }

    @Override
    public final boolean isConcreteCollection() {
        return collectionVariables.isEmpty();
    }

    @Override
    public boolean isExactSort() {
        return true;
    }

    @Override
    public Sort sort() {
        return kind.asSort();
    }

    @Override
    public boolean equals(Object object) {
        if (this == object) {
            return true;
        }

        if (!(object instanceof CellCollection)) {
            return false;
        }

        CellCollection collection = (CellCollection) object;
        if (!collectionVariables.equals(collection.collectionVariables)) {
            return false;
        }

        if (cells.keys().equals(collection.cells.keys())) {
            for (String key : cells.keySet()) {
                /* YilongL: didn't use the O(N) hash-based approach for
                 * comparing two bags because 1) the fast destructive rewriter
                 * destroys hashcode of terms 2) the number of elements is
                 * usually very small */
                List<Cell> list = Lists.newArrayList(cells.get(key));
                for (Cell<?> c : collection.cells.get(key)) {
                    if (!list.remove(c)) {
                        return false;
                    }
                }
            }
            return true;
        } else {
            return false;
        }
    }

    @Override
    protected int computeHash() {
        int hashCode = 1;
        hashCode = hashCode * Utils.HASH_PRIME + cells.hashCode();
        hashCode = hashCode * Utils.HASH_PRIME + collectionVariables.hashCode();
        return hashCode;
    }

    @Override
    protected boolean computeMutability() {
        return true;
    }

    @Override
    public String toString() {
        if (isEmpty()) {
            return DataStructureSort.LABELS.get(org.kframework.kil.Sort.BAG).get(Label.UNIT);
        } else {
            StringBuilder stringBuilder = new StringBuilder();
            for (Cell cell : cells.values()) {
                stringBuilder.append(cell);
            }
            Iterator<Term> iter = baseTerms().iterator();
            while (iter.hasNext()) {
                stringBuilder.append(iter.next());
                if (iter.hasNext()) {
                    stringBuilder.append(" ");
                }
            }
            return stringBuilder.toString();
        }
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

    public static Builder builder(Context context) {
        return new Builder(context);
    }

    public static class Builder {

        private final ImmutableListMultimap.Builder<String, Cell> cellsBuilder = ImmutableListMultimap.builder();

        private final ImmutableMultiset.Builder<Variable> collectionVariablesBuilder = ImmutableMultiset.builder();

        private final Context context;

        private Builder(Context context) {
            this.context = context;
        }

        public void add(Cell cell) {
            cellsBuilder.put(cell.getLabel(), cell);
        }

        public void putAll(ListMultimap<String, Cell> cellMap) {
            cellsBuilder.putAll(cellMap);
        }

        public void putAll(String cellLabel, java.util.Collection<Cell> cells) {
            cellsBuilder.putAll(cellLabel, cells);
        }

        public void concatenate(Term term) {
            if (term instanceof CellCollection) {
                CellCollection cellCol = (CellCollection) term;
                cellsBuilder.putAll(cellCol.cells);
                collectionVariablesBuilder.addAll(cellCol.collectionVariables);
            } else if (term instanceof Cell) {
                add((Cell) term);
            } else if (term instanceof Variable) {
                collectionVariablesBuilder.add((Variable) term);
            } else {
                assert false : "unexpected concatenated term " + term;
            }
        }

        public Term build() {
            ImmutableListMultimap<String, Cell> cells = cellsBuilder.build();
            ImmutableMultiset<Variable> collectionVariables = collectionVariablesBuilder.build();
            if (cells.isEmpty()) {
                switch (collectionVariables.size()) {
                    case 0:  return EMPTY;
                    case 1:  return collectionVariables.iterator().next();
                    default: return new CellCollection(cells, collectionVariables, context);
                }
            } else {
                if (cells.size() == 1 && collectionVariables.isEmpty()) {
                    return cells.values().iterator().next();
                } else {
                    return new CellCollection(cells, collectionVariables, context);
                }
            }
        }
    }


    /**
     * Promotes a given {@link Term} to a given {@link Kind}. The {@code Kind}s
     * involved in this method can only be {@code Kind#CELL} or
     * {@code Kind#CELL_COLLECTION}. If the kind of the given {@code Term} is
     * already above or equal to the target {@code Kind}, do nothing.
     * <p>
     * To be more specific, a {@code Cell} can be promoted to a single-element
     * {@code CellCollection}.
     *
     * @param term
     *            the given term to be promoted
     * @param kind
     *            the target kind that the term is to be promoted to
     * @return the resulting term after kind promotion
     */
    public static Term upKind(Term term, Kind kind, Context context) {
        assert term.kind() == Kind.CELL || term.kind() == Kind.CELL_COLLECTION;
        assert kind == Kind.CELL || kind == Kind.CELL_COLLECTION;

        /* promote Cell to CellCollection */
        if (term instanceof Cell && kind == Kind.CELL_COLLECTION) {
            return CellCollection.singleton((Cell) term, context);
        }

        return term;
    }

    /**
     * Degrades a given {@link Term} to a given {@link Kind}. The {@code Kind}s
     * involved in this method can only be {@code Kind#CELL} or
     * {@code Kind#CELL_COLLECTION}. If the kind of the given {@code Term} is
     * already lower than or equal to the target {@code Kind}, do nothing.
     * <p>
     * To be more specific, a single-element {@code CellCollection} can be
     * degraded to a {@code Cell}.
     *
     * @param term
     *            the given term to be degraded
     * @return the resulting term after kind degradation
     */
    public static Term downKind(Term term) {
        assert term.kind() == Kind.CELL || term.kind() == Kind.CELL_COLLECTION;

        if (term instanceof CellCollection
                && ((CellCollection) term).baseTerms().isEmpty()
                && ((CellCollection) term).concreteSize() == 1) {
            term = ((CellCollection) term).cells().iterator().next();
        }

        // YilongL: do not degrade the kind of a Variable since you cannot
        // upgrade it later

        return term;
    }

}
