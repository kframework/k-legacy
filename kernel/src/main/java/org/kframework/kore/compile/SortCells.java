// Copyright (c) 2015 K Team. All Rights Reserved.
package org.kframework.kore.compile;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Sets;
import org.kframework.builtin.BooleanUtils;
import org.kframework.compile.ConfigurationInfo;
import org.kframework.compile.ConfigurationInfo.Multiplicity;
import org.kframework.compile.LabelInfo;
import org.kframework.definition.Context;
import org.kframework.definition.Rule;
import org.kframework.definition.Sentence;
import org.kframework.kil.Attribute;
import org.kframework.kore.K;
import org.kframework.kore.KApply;
import org.kframework.kore.KLabel;
import org.kframework.kore.KRewrite;
import org.kframework.kore.KVariable;
import org.kframework.kore.Sort;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.kframework.Collections.*;
import static org.kframework.definition.Constructors.*;
import static org.kframework.kore.KORE.*;

/**
 * Arrange cell contents and variables to match the klabels declared for cells.
 * In Full K, cell contents can be written in any order, and variables can
 * be written that match multiple cells.
 * <p>
 * In the input to this pass, parent cells are represented by appling the label directly
 * to a klist of all the children, variables, and rewrites under the cell.
 * Left cells should already be in their final form.
 * In the output each cell will be represented by using the cell labels in agreement
 * with the production declaring it, so parent cells will have a fixed arity with separate
 * argument positions reserved for different types of child cell.
 * <p>
 * The most complicated part of the transformation is dealing with cell fragment
 * variables appearing under explicit cell or cell fragment labels.
 * These occurances must be replaced with a collection of variables placed
 * in the correct arguments of the parent label, and any occurances of that
 * variable outside of an existing parent label must be replaced with
 * a suitable cell fragment term, using the same split variables.
 */
public class SortCells {
    private final ConfigurationInfo cfg;
    private final LabelInfo labels;
    private final KExceptionManager kem;

    public SortCells(ConfigurationInfo cfgInfo, LabelInfo labelInfo, KExceptionManager kem) {
        this.cfg = cfgInfo;
        this.labels = labelInfo;
        this.kem = kem;
    }

    private Map<KVariable, VarInfo> variableInfo = new HashMap<>();
    private Map<KVariable, VarSplitInfo> variableSplits = new HashMap<>();
    private Map<KVariable, Sort> cellVariables = new HashMap<>();
    private Set<KVariable> usedVars = new LinkedHashSet<>();

    private void resetVars() {
        variableInfo.clear();
        variableSplits.clear();
        cellVariables.clear();
        usedVars.clear();
        counter = 0;
    }

    private void calculateSplits() {
        List<KVariable> varOrder = new ArrayList<>(usedVars);
        for (KVariable var : varOrder) {
            if (variableInfo.containsKey(var)) {
                variableSplits.put(var, new VarSplitInfo(var, variableInfo.get(var)));
            }
        }
    }

    private int counter = 0;

    KVariable newDotVariable() {
        KVariable newLabel;
        do {
            newLabel = KVariable("_" + (counter++));
        } while (usedVars.contains(newLabel));
        usedVars.add(newLabel);
        return newLabel;
    }

    public synchronized K sortCells(K term) {
        resetVars();
        analyzeVars(term);
        calculateSplits();
        return processVars(term);
    }

    private Rule sortCells(Rule rule) {
        resetVars();
        analyzeVars(rule.body());
        analyzeVars(rule.requires());
        analyzeVars(rule.ensures());
        calculateSplits();
        rule = Rule(
                processVars(rule.body()),
                processVars(rule.requires()),
                processVars(rule.ensures()),
                rule.att());
        return rule;
    }

    private Context sortCells(Context context) {
        resetVars();
        analyzeVars(context.body());
        analyzeVars(context.requires());
        calculateSplits();
        return new Context(
                processVars(context.body()),
                processVars(context.requires()),
                context.att());
    }

    public synchronized Sentence sortCells(Sentence s) {
        if (s instanceof Rule) {
            return sortCells((Rule) s);
        } else if (s instanceof Context) {
            return sortCells((Context) s);
        } else {
            return s;
        }
    }

    // Information on uses of a particular cell fragment variable
    private class VarInfo {
        KVariable var;
        Sort parentCell;
        // if this variable is only used inside cell fragments,
        // and it's split needs to include a variable for a
        // multiplicity one cell, that variable should be
        // sorted as that optional sort which is used
        // in the declaration of the fragment.
        boolean onlyFragment = true;
        LinkedHashSet<Sort> remainingCells;

        void addOccurances(KLabel parentLabel, KVariable var, List<K> items) {
            this.var = var;
            Sort parentSort = labels.getCodomain(parentLabel);
            Sort cell;
            if (cfg.isCell(parentSort)) {
                cell = parentSort;
                onlyFragment = false;
            } else {
                cell = cfg.getCellOfFragment(parentSort);
            }
            if (parentCell == null) {
                parentCell = cell;
            } else if (!parentCell.equals(cell)) {
                throw KEMException.criticalError("Cell variable " + var + " used under two cells, "
                        + parentCell + " and " + cell);
            }
            if (remainingCells == null) {
                remainingCells = new LinkedHashSet<>(cfg.getChildren(cell));
            }
            if (var.att().contains(Attribute.SORT_KEY)) {
                Sort sort = Sort(var.att().<String>get(Attribute.SORT_KEY).get());
                sort = cfg.getCellOfFragmentMember(sort);
                if (sort != null) {
                    remainingCells.retainAll(ImmutableList.of(sort));
                }
            }
            for (K item : items) {
                Sort s;
                if (item instanceof KApply) {
                    KApply kApply = (KApply) item;
                    s = labels.getCodomain(kApply.klabel());
                } else if (item instanceof KVariable && !item.equals(var)
                        && item.att().contains(Attribute.SORT_KEY)) {
                    s = Sort(item.att().<String>get(Attribute.SORT_KEY).get());
                } else {
                    s = null;
                }
                s = cfg.getCellOfFragmentMember(s);
                if (s != null && cfg.getMultiplicity(s) != Multiplicity.STAR) {
                    remainingCells.remove(s);
                }
            }
        }
    }

    class VarSplitInfo {
        boolean onlyFragment;
        Map<Sort, K> split;
        K replacementTerm;

        VarSplitInfo(KVariable var, VarInfo info) {
            onlyFragment = info.onlyFragment;

            if (info.remainingCells.size() == 0) {
                split = Collections.emptyMap();
            } else if (info.remainingCells.size() == 1) {
                Sort s = Iterables.getOnlyElement(info.remainingCells);
                split = ImmutableMap.of(s, KVariable(var.name(), var.att().remove(Attribute.SORT_KEY)));
            } else {
                split = new HashMap<>();
                for (Sort cell : info.remainingCells) {
                    split.put(cell, newDotVariable());
                }
            }

            KLabel fragmentLabel = cfg.getCellFragmentLabel(info.parentCell);
            if (fragmentLabel == null) {
                throw KEMException.compilerError("Unsupported cell fragment with types: " + info.remainingCells, var);
            }
            List<Sort> children = cfg.getChildren(info.parentCell);
            List<K> arguments = new ArrayList<>(children.size());
            for (Sort child : children) {
                K arg = split.get(child);
                if (arg == null) {
                    if (cfg.getMultiplicity(child) == Multiplicity.ONE) {
                        KLabel l = cfg.getCellAbsentLabel(child);
                        arg = KApply(l);
                    } else {
                        arg = cfg.getUnit(child);
                    }
                }
                assert arg != null;
                arguments.add(arg);
            }
            replacementTerm = KApply(fragmentLabel, immutable(arguments));
        }

        Map<Sort, K> getSplit() {
            return split;
        }

        K replacementTerm() {
            return replacementTerm;
        }
    }

    private void analyzeVars(K term) {
        new VisitKORE() {
            private boolean inRewrite = false;
            private boolean inRhs = false;

            private Stream<K> streamCells(K term) {
                return IncompleteCellUtils.flattenCells(term).stream();
            }

            private K left(K term) {
                if (term instanceof KRewrite) {
                    return ((KRewrite) term).left();
                } else {
                    return term;
                }
            }

            private K right(K term) {
                if (term instanceof KRewrite) {
                    return ((KRewrite) term).right();
                } else {
                    return term;
                }
            }

            @Override
            public Void apply(KApply k) {
                final Sort codomain = labels.getCodomain(k.klabel());
                if (cfg.isParentCell(codomain) || cfg.isCellFragment(codomain)) {
                    if (inRewrite) {
                        processSide(k, inRhs, k.klist().stream()
                                .flatMap(this::streamCells)
                                .collect(Collectors.toList()));
                    } else {
                        processSide(k, false, k.klist().stream()
                                .flatMap(this::streamCells).map(this::left).flatMap(this::streamCells)
                                .collect(Collectors.toList()));

                        processSide(k, true, k.klist().stream()
                                .flatMap(this::streamCells).map(this::right).flatMap(this::streamCells)
                                .collect(Collectors.toList()));
                    }
                }
                return super.apply(k);
            }

            private void processSide(KApply parentCell, boolean allowRhs, List<K> items) {
                List<KVariable> bagVars = new ArrayList<>();
                for (K item : items) {
                    if (item instanceof KVariable) {
                        KVariable var = (KVariable) item;
                        if (var.att().contains(Attribute.SORT_KEY)) {
                            Sort sort = Sort(var.att().<String>get(Attribute.SORT_KEY).get());
                            if (cfg.isCell(sort) || (cfg.getCellOfFragmentMember(sort) != null)) {
                                if (!cellVariables.getOrDefault(var, sort).equals(sort)) {
                                    Sort prevSort = cellVariables.get(var);
                                    throw KEMException.compilerError("Variable " + var + " occurs annotated as multiple cell sorts, " + sort + " and " + prevSort,
                                            item);
                                }
                                if (variableInfo.containsKey(var)) {
                                    throw KEMException.compilerError("Variable " + var + " occurs with cell sort " + sort + " after occurance without cell sort annotation");
                                }
                                cellVariables.put(var, sort);
                                continue;
                            } else {
                                if (cellVariables.containsKey(var)) {
                                    throw KEMException.compilerError("Variable " + var + " occurs annotated as non-cell sort " + sort + " after appearing as cell sort " + cellVariables.get(var), item);
                                }
                            }
                        }
                        if (cellVariables.containsKey(var)) {
                            throw KEMException.compilerError("Variable " + var + " occurs without sort annotation after appearing as cell sort " + cellVariables.get(var), item);
                        }
                        bagVars.add(var);
                    }
                }
                if (!allowRhs && bagVars.size() > 1) {
                    throw KEMException.compilerError(
                            "AC matching of multiple cell variables not yet supported. "
                                    + "encountered variables " + bagVars + " in cell " + parentCell.klabel(), parentCell);
                }
                for (KVariable var : bagVars) {
                    if (!variableInfo.containsKey(var)) {
                        variableInfo.put(var, new VarInfo());
                    }
                    variableInfo.get(var).addOccurances(parentCell.klabel(), var, items);
                }
            }

            @Override
            public Void apply(KRewrite k) {
                assert !inRewrite;
                inRewrite = true;
                apply(k.left());
                inRhs = true;
                apply(k.right());
                inRhs = false;
                inRewrite = false;
                return null;
            }

            @Override
            public Void apply(KVariable k) {
                usedVars.add(k);
                return null;
            }
        }.apply(term);
    }

    private KLabel getPredicate(boolean onlyFragment, Sort s) {
        return KLabel("is"+getPredicateSort(onlyFragment,s));
    }

    private Sort getPredicateSort(boolean onlyFragment, Sort s) {
        switch (cfg.getMultiplicity(s)) {
        case STAR:
            return cfg.getCellBagSortOfCell(s);
        case OPTIONAL:
            return s;
        case ONE:
            if (onlyFragment) {
                return cfg.getCellOptOfCell(s);
            } else {
                return s;
            }
        default:
            return s;
        }
    }

    private boolean isCellFragmentTest(KApply app) {
        if (app.klist().size() != 1) return false;
        K argument = app.klist().items().get(0);
        if (!(argument instanceof KVariable)) return false;
        VarInfo info = variableInfo.get((KVariable) argument);
        if (info == null) return false;
        KLabel expectedPredicate = KLabel("is" + cfg.getFragmentOfCell(info.parentCell).toString());
        return app.klabel().equals(expectedPredicate);
    }

    /**
     * Expand away cell fragment variables, and correctly order the children of cells.
     * There are three significnat contexts for expanding cell fragment variables -
     * A use as an argument to a parent cell or cell fragment label it splits into
     * a separate variables for child cells.
     * A term applying the sort predicate for the cell fragment sort to the original variable
     * splits into a conjunction of sort predicate tests on the variables the cell fragment
     * variable split into.
     * In other uses, it expands into an application of the appropriate cell-fragment label
     * to variables from the splits.
     */
    private K processVars(K term) {
        return new TransformKORE() {
            @Override
            public K apply(KApply k) {
                Sort resultSort = labels.getCodomain(k.klabel());
                if (cfg.isParentCell(resultSort) || cfg.isCellFragment(resultSort)) {
                    boolean isFragment = cfg.isCellFragment(resultSort);
                    Sort cell = isFragment ? cfg.getCellOfFragment(resultSort) : resultSort;
                    List<Sort> order = cfg.getChildren(cell);
                    ArrayList<K> ordered = new ArrayList<K>(Collections.nCopies(order.size(), null));
                    for (K item : k.klist().items()) {
                        Map<Sort, K> split = getSplit(item);
                        for (Map.Entry<Sort, K> e : split.entrySet()) {
                            Sort entryCell = e.getKey();
                            if (!cfg.isCell(entryCell)) {
                                entryCell = cfg.getCellOfFragmentMember(entryCell);
                            }
                            int idx = order.indexOf(entryCell);
                            if (ordered.get(idx) != null) {
                                ordered.set(idx, concatenateStarCells(entryCell, Arrays.asList(ordered.get(idx), e.getValue())));
                            } else {
                                ordered.set(idx, e.getValue());
                            }
                        }
                    }
                    order.stream().filter(s -> ordered.get(order.indexOf(s)) == null).forEach(sort -> {
                        if (cfg.getMultiplicity(sort) == Multiplicity.ONE) {
                            if (isFragment) {
                                ordered.set(order.indexOf(sort), KApply(cfg.getCellAbsentLabel(sort)));
                            } else {
                                throw KEMException.compilerError("Missing cell of multiplicity=\"1\": " + sort, k);
                            }
                        } else {
                            ordered.set(order.indexOf(sort), cfg.getUnit(sort));
                        }
                    });
                    return KApply(k.klabel(), KList(ordered), k.att());
                } else {
                    if (isCellFragmentTest(k)) {
                        VarSplitInfo info = variableSplits.get(k.klist().items().get(0));
                        final boolean onlyFragment = info.onlyFragment;
                        return info.getSplit().entrySet().stream()
                                .map(e -> (K) KApply(getPredicate(onlyFragment, e.getKey()), e.getValue()))
                                .reduce(BooleanUtils.TRUE, BooleanUtils::and);
                    } else if (k.klabel().name().equals("isBag")
                            && k.klist().size() == 1
                            && k.klist().items().get(0) instanceof KVariable) {
                        KVariable var = (KVariable) k.klist().items().get(0);
                        VarSplitInfo info = variableSplits.get(var);
                        if (info != null) {
                            final boolean onlyFragment = info.onlyFragment;
                            return info.getSplit().entrySet().stream()
                                    .map(e -> (K) KApply(getPredicate(onlyFragment, e.getKey()), e.getValue()))
                                    .reduce(BooleanUtils.TRUE, BooleanUtils::and);
                        }
                    }
                    return super.apply(k);
                }
            }

            @Nonnull
            private Map<Sort, K> getSplit(K item) {
                if (item instanceof KVariable) {
                    VarSplitInfo info = variableSplits.get(item);
                    if (info == null) {
                        Sort cellSort = cellVariables.get(item);
                        if (cellSort == null) {
                            throw new IllegalArgumentException("Unknown variable " + item);
                        } else {
                            return ImmutableMap.of(cellSort, item);
                        }
                    }
                    return info.getSplit();
                } else if (item instanceof KApply) {
                    List<K> children = IncompleteCellUtils.flattenCells(item);
                    if (children.size() == 1 && children.get(0) == item) {
                        final KLabel label = ((KApply) item).klabel();
                        Sort cell = cfg.getCellOfFragmentMember(labels.getCodomain(label));
                        if (cell == null) {
                            throw new IllegalArgumentException("Attempting to split non-cell term " + item);
                        }
                        return Collections.singletonMap(cell, apply(item));
                    }
                    // flatten the List<Map<Sort, K>> into a Map<Sort, List<K>>
                    Map<Sort, List<K>> multiMap = children.stream().flatMap(e -> getSplit(e).entrySet().stream()).collect(
                            Collectors.groupingBy(Map.Entry::getKey,
                                    Collectors.mapping(Map.Entry::getValue, Collectors.toList())));
                    return multiMap.entrySet().stream().filter(e -> e.getValue().size() > 0).collect(Collectors.toMap(e -> e.getKey(), e -> {
                        if (e.getValue().size() == 1) {
                            return e.getValue().get(0);
                        } else {
                            return concatenateStarCells(e.getKey(), e.getValue());
                        }
                    }));
                } else if (item instanceof KRewrite) {
                    KRewrite rw = (KRewrite) item;
                    Map<Sort, K> splitLeft = new HashMap<>(getSplit(rw.left()));
                    Map<Sort, K> splitRight = new HashMap<>(getSplit(rw.right()));
                    addDefaultCells(item, splitLeft, splitRight);
                    addDefaultCells(item, splitRight, splitLeft);
                    assert splitLeft.keySet().equals(splitRight.keySet());
                    return splitLeft.keySet().stream().collect(Collectors.toMap(sort -> sort,
                            sort -> KRewrite(splitLeft.get(sort), splitRight.get(sort), rw.att())));
                } else {
                    throw KEMException.compilerError("Unexpected kind of term found in cell. Expected variable, "
                            + "apply, or rewrite; found " + item.getClass().getSimpleName(), item);
                }
            }

            private K concatenateStarCells(Sort sort, List<K> children) {
                if (cfg.getMultiplicity(sort) != Multiplicity.STAR) {
                    throw KEMException.compilerError("Attempting to concatenate cells " + sort
                                    + " not of multiplicity=\"*\" into a cell collection.",
                            children.iterator().next());
                }
                if (children.size() == 0) {
                    return cfg.getUnit(sort);
                }
                KLabel concat = cfg.getConcat(sort);
                int ix = children.size();
                K result = children.get(--ix);
                while (ix > 0) {
                    result = KApply(concat, children.get(--ix), result);
                }
                return result;
            }

            private void addDefaultCells(K item, Map<Sort, K> splitLeft, Map<Sort, K> splitRight) {
                for (Sort s : Sets.difference(splitLeft.keySet(), splitRight.keySet())) {
                    if (cfg.getMultiplicity(s) == Multiplicity.ONE) {
                        throw KEMException.compilerError("Cannot rewrite a multiplicity=\"1\" cell to or from the cell unit.", item);
                    } else {
                        splitRight.put(s, cfg.getUnit(s));
                    }
                }
            }

            @Override
            public K apply(KVariable v) {
                VarSplitInfo info = variableSplits.get(v);
                if (info != null) {
                    return info.replacementTerm();
                } else {
                    return v;
                }
            }
        }.apply(term);
    }
}
