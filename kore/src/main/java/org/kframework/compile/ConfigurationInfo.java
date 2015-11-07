// Copyright (c) 2015 K Team. All Rights Reserved.
package org.kframework.compile;

import org.kframework.kore.K;
import org.kframework.kore.KApply;
import org.kframework.kore.KLabel;
import org.kframework.kore.Sort;
import scala.Option;

import java.util.List;
import java.util.Set;

/**
 * Information about the configuration structure used
 * in the configuration concretiziation passes.
 * Cells are identified by the sort containing the production
 * for the cell (this is more convenient when dealing also
 * with variables or functions of cell sort).
 */
public interface ConfigurationInfo {
    /**
     * Number of proper ancestors of cell k
     */
    int getLevel(Sort k);

    /**
     * Parent of cell k, or null
     */
    Sort getParent(Sort k);

    /**
     * Children of cell k
     */
    List<Sort> getChildren(Sort k);

    /**
     * Declared multiplicity of a cell
     */
    Multiplicity getMultiplicity(Sort k);

    /**
     * True if sort k is actually a cell sort
     */
    boolean isCell(Sort k);

    /**
     * True if sort s is the collection sort for a multiplicity star cell.
     */
    boolean isCellCollection(Sort s);

    /**
     * True if sort s is the optional version (for fragments) of a multiplicity one cell.
     */
    boolean isCellOpt(Sort s);

    /**
     * Return the corresponding cell sort if {@code s} is
     * the supersort of a multiplicity one cell which makes the
     * cell optional, which is used to define the parent cell's fragment.
     */
    Sort getCellOfOpt(Sort s);
    /**
     * Given the sort of a multiplicity one cell, return the
     * supersort which makes the cell optional, which is used
     * as an argument sort in the parent cell's fragment.
     */
    Sort getCellOptOfCell(Sort s);

    /**
     * Return the corresponding cell sort, if the argument sort is
     * a cell or cell fragment sort.
     * Returns null otherwise.
     */
    Sort getCellOfFragment(Sort s);

    /**
     * Return the corresponding cell fragment sort, if the argument
     * is a parent cell's sort, or already a cell fragment sort.
     */
    Sort getFragmentOfCell(Sort s);

    /**
     * Return the corresponding cell sort, if the argument sort is
     * a collection or optional sort used in cell fragment, or already a cell sort.
     * Returns null otherwise.
     */
    Sort getCellOfFragmentMember(Sort s);

    /**
     * True if kLabel is the KLabel of a cell
     */
    boolean isCellLabel(KLabel kLabel);

    /**
     * True for cells which contain a term rather than other cells
     */
    boolean isLeafCell(Sort k);

    /**
     * True for cells which contain other cells
     */
    boolean isParentCell(Sort k);

    /**
     * True for cell fragment sorts
     */
    boolean isCellFragment(Sort k);

    /**
     * The cell bag sort (e.g. ThreadCellBag) associated with a multiplicity * cell (e.g. ThreadCell).
     */
    Sort getCellBagSortOfCell(Sort k);

    /**
     * The declared sort of the contents of a leaf cell
     */
    Sort leafCellType(Sort k);

    /**
     * The label for a cell
     */
    KLabel getCellLabel(Sort k);

    /**
     * The cell for a label
     */
    Sort getCellSort(KLabel kLabel);

    /**
     * The label for a fragment of a cell, only defined for parent cells.
     */
    KLabel getCellFragmentLabel(Sort k);

    /**
     * The constant label to use as an argument of a cell fragment,
     * when the cell fragment did not capture cells of the argument type.
     * Only defined for child cells of multiplicity other than *.
     */
    KLabel getCellAbsentLabel(Sort cellSort);

    /**
     * Returns a term which is the default cell of sort k,
     * probably an initializer macro
     */
    K getDefaultCell(Sort k);

    /**
     * True if the initializer for cell sort k does not
     * depend on any configuration variables
     */
    boolean isConstantInitializer(Sort k);

    /**
     * Return the root cell of the configuration .
     */
    Sort getRootCell();

    /**
     * Return the declared computation cell, by default the k cell
     */
    Sort getComputationCell();

    /**
     * Returns the set of cell sorts in this configuration
     */
    Set<Sort> getCellSorts();

    /**
     * Returns the unit of a * or ? cell.
     */
    KApply getUnit(Sort k);

    /**
     * Returns the concatenation operation of a multiplicity * cell.
     */
    KLabel getConcat(Sort k);

    /**
     * Returns the cell associated with this concatenation label
     */
    Option<Sort> getCellForConcat(KLabel concat);

    /**
     * Returns the cell associated with this unit
     */
    Option<Sort> getCellForUnit(KLabel unit);

    /**
     * If {@code label} is a label making a cell collection, return the
     * Sort of the cells in that collection.
     */
    default public Sort getCellCollectionCell(KLabel label) {
        Option<Sort> result = getCellForConcat(label);
        if (result.isEmpty()) {
            result = getCellForUnit(label);
        }
        return result.isDefined() ? result.get() : null;
    }

    /**
     * Declared mulitplicitly of a cell
     */
    enum Multiplicity {
        /**
         * Exactly one instance of this cell is required
         */
        ONE,
        /**
         * This cell is optional but may not be repeated
         */
        OPTIONAL,
        /**
         * This cell may occur any number of times, zero or more
         */
        STAR;

        public static Multiplicity of(String multiplicity) {
            switch (multiplicity) {
            case "1":
                return ConfigurationInfo.Multiplicity.ONE;
            case "*":
                return ConfigurationInfo.Multiplicity.STAR;
            case "?":
                return ConfigurationInfo.Multiplicity.OPTIONAL;
            default:
                throw new IllegalArgumentException(multiplicity);
            }
        }
    }
}
