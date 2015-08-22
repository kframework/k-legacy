// Copyright (c) 2014-2015 K Team. All Rights Reserved.
package org.kframework.kil.loader;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.SetMultimap;
import com.google.common.collect.Sets;
import com.google.inject.Inject;
import org.kframework.compile.utils.ConfigurationStructureMap;
import org.kframework.kil.Attribute;
import org.kframework.kil.Attribute.Key;
import org.kframework.kil.Cell;
import org.kframework.kil.DataStructureSort;
import org.kframework.kil.Production;
import org.kframework.kil.Sort;
import org.kframework.kompile.KompileOptions;
import org.kframework.krun.KRunOptions;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.Poset;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.inject.RequestScoped;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

@RequestScoped
public class Context implements Serializable {

    public static final Set<Key<String>> parsingTags = ImmutableSet.of(
            Attribute.keyOf("left"),
            Attribute.keyOf("right"),
            Attribute.keyOf("non-assoc"));

    public static final Set<String> specialTerminals = ImmutableSet.of(
        "(",
        ")",
        ",",
        "[",
        "]",
        "{",
        "}");

    /**
     * Represents the bijection map between conses and productions.
     */
    public Set<Production> productions = new HashSet<>();
    /**
     * Represents a map from all Klabels in string representation
     * to sets of corresponding productions.
     * why?
     */
    public SetMultimap<String, Production> klabels = HashMultimap.create();
    public SetMultimap<String, Production> tags = HashMultimap.create();
    public Map<String, Cell> cells = new HashMap<>();
    public Map<Sort, Production> listProductions = new LinkedHashMap<>();
    public SetMultimap<String, Production> listKLabels = HashMultimap.create();

    private Poset<Sort> subsorts = Poset.create();
    private Poset<Sort> syntacticSubsorts = Poset.create();
    public Map<String, Sort> configVarSorts = new HashMap<>();
    public HashMap<Sort, String> freshFunctionNames = new HashMap<>();
    public HashMap<Sort, Sort> smtSortFlattening = new HashMap<>();

    private BiMap<String, Production> conses;

    /**
     * The two structures below are populated by the InitializeConfigurationStructure step of the compilation.
     * configurationStructureMap represents a map from cell names to structures containing cell data.
     * maxConfigurationLevel represent the maximum level of cell nesting in the configuration.
     */
    private ConfigurationStructureMap configurationStructureMap = new ConfigurationStructureMap();
    private int maxConfigurationLevel = -1;

    /**
     * {@link Map} of sort names into {@link DataStructureSort} instances.
     */
    private Map<Sort, DataStructureSort> dataStructureSorts;

    /**
     * {@link Set} of sorts with lexical productions.
     */
    private Set<Sort> tokenSorts;


    public ConfigurationStructureMap getConfigurationStructureMap() {
        return configurationStructureMap;
    }

    private void initSubsorts(Poset<Sort> subsorts) {
        subsorts.addElement(Sort.KLABEL);
        subsorts.addRelation(Sort.KLIST, Sort.K);
        subsorts.addRelation(Sort.K, Sort.KITEM);
        subsorts.addRelation(Sort.KITEM, Sort.KRESULT);
        subsorts.addRelation(Sort.BAG, Sort.BAG_ITEM);
    }

    // TODO(dwightguth): remove these fields and replace with injected dependencies
    @Deprecated @Inject public transient GlobalOptions globalOptions;
    public KompileOptions kompileOptions;
    @Deprecated @Inject(optional=true) public transient KRunOptions krunOptions;

    public Context() {
        initSubsorts(subsorts);
        initSubsorts(syntacticSubsorts);
    }

    public Sort startSymbolPgm() {
        return configVarSorts.getOrDefault("PGM", Sort.K);
    }

    public void addProduction(Production p) {
        productions.add(p);
        if (p.getKLabel() != null) {
            klabels.put(p.getKLabel(), p);
            tags.put(p.getKLabel(), p);
            if (p.isListDecl()) {
                listKLabels.put(p.getTerminatorKLabel(), p);
            }
        }
        if (p.isListDecl()) {
            listProductions.put(p.getSort(), p);
        }
        for (Attribute<?> a : p.getAttributes().values()) {
            tags.put(a.getKey().toString(), p);
        }
    }

    /**
     * Returns a unmodifiable view of all sorts.
     */
    public Set<Sort> getAllSorts() {
        return Collections.unmodifiableSet(subsorts.getElements());
    }

    /**
     * Finds the LUB (Least Upper Bound) of a given set of sorts.
     *
     * @param sorts
     *            the given set of sorts
     * @return the sort which is the LUB of the given set of sorts on success;
     *         otherwise {@code null}
     */
    public Sort getLUBSort(Sort... sorts) {
        return subsorts.getLUB(Sets.newHashSet(sorts));
    }

    public void addSubsort(Sort bigSort, Sort smallSort) {
        subsorts.addRelation(bigSort, smallSort);
    }

    /**
     * Check to see if smallSort is subsorted to bigSort (strict)
     */
    public boolean isSubsorted(Sort bigSort, Sort smallSort) {
        return subsorts.isInRelation(bigSort, smallSort);
    }

    /**
     * Check to see if smallSort is subsorted or equal to bigSort
     */
    public boolean isSubsortedEq(Sort bigSort, Sort smallSort) {
        if (bigSort.equals(smallSort))
            return true;
        return subsorts.isInRelation(bigSort, smallSort);
    }

    public boolean isSpecialTerminal(String terminal) {
        return specialTerminals.contains(terminal);
    }

    public boolean isParsingTag(Key<?> key) {
        return parsingTags.contains(key);
    }

    public static final int HASH_PRIME = 37;

    /**
     * Returns a {@link Set} of productions associated with the specified KLabel
     *
     * @param label
     *            string representation of the KLabel
     * @return list of productions associated with the label
     */
    public Set<Production> productionsOf(String label) {
        return klabels.get(label);
    }

    public Map<Sort, DataStructureSort> getDataStructureSorts() {
        return Collections.unmodifiableMap(dataStructureSorts);
    }

    /**
     * Return the DataStructureSort corresponding to the given Sort, or null if
     * the sort is not known as a data structure sort.
     */
    public DataStructureSort dataStructureSortOf(Sort sort) {
        return dataStructureSorts.get(sort);
    }

    /**
     * Like dataStructureSortOf, except it returns null also if
     * the sort corresponds to a DataStructureSort which isn't a list sort.
     */
    public DataStructureSort dataStructureListSortOf(Sort sort) {
        DataStructureSort dataStructSort = dataStructureSorts.get(sort);
        if (dataStructSort == null) return null;
        if (!dataStructSort.type().equals(Sort.LIST)) return null;
        return dataStructSort;
    }

    /**
     * Get a DataStructureSort for the default list sort, or raise a nice exception.
     * Equivalent to
     * <code>dataStructureListSortOf(DataStructureSort.DEFAULT_LIST_SORT)</code>,
     * if it succeeds.
     */
    public DataStructureSort getDefaultListDataStructureSort() {
        DataStructureSort list = dataStructureListSortOf(DataStructureSort.DEFAULT_LIST_SORT);
        if (list == null) {
            throw KEMException.internalError(
                    "A sort List must exist and be recognized as a data structure sort."
                            + " Installation is corrupt or --no-prelude used with incomplete definition.");
        }
        return list;
    }

    /**
     * Returns the set of sorts that have lexical productions.
     */
    public Set<Sort> getTokenSorts() {
        return Collections.unmodifiableSet(tokenSorts);
    }

    public void makeSMTSortFlatteningMap(Set<Production> freshProductions) {
        for (Production production : freshProductions) {
            if (!production.isSubsort()) {
                throw KExceptionManager.compilerError(
                        "unexpected tag [smt-sort-flatten] for non-subsort production " + production,
                        production);
            }

            smtSortFlattening.put(production.getSubsort(), production.getSort());
        }
    }


}
