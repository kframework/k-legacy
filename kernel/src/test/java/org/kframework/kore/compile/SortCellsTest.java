// Copyright (c) 2015 K Team. All Rights Reserved.
package org.kframework.kore.compile;

import org.junit.Assert;
import org.junit.Test;
import org.kframework.builtin.BooleanUtils;
import org.kframework.builtin.KLabels;
import org.kframework.builtin.Sorts;
import org.kframework.compile.ConfigurationInfo;
import org.kframework.compile.LabelInfo;
import org.kframework.definition.Rule;
import org.kframework.kil.Attribute;
import org.kframework.kore.K;
import org.kframework.kore.KApply;
import org.kframework.kore.KVariable;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.errorsystem.KExceptionManager;

import static org.kframework.kore.KORE.*;

/**
 * Rearrange partially-completed cells to follow the productions declaring them.
 * <p>
 * The main complexity here is eliminating cell fragment variables that might
 * capture multiple cells. In the general case a variable needs to be
 * replaced under cells with separate variables in several slots of the
 * parent and in other places with an appropriate bag expression.
 */
public class SortCellsTest {

    ConfigurationInfo cfgInfo = new TestConfiguration() {{
        addCell(null, "TopCell", "<top>");
        addCell("TopCell", "ThreadCell", "<t>", Multiplicity.STAR);
        addCell("ThreadCell", "KCell", "<k>", Sorts.K());
        addCell("ThreadCell", "EnvCell", "<env>", Sort("Map"));
        addCell("ThreadCell", "OptCell", "<opt>", Multiplicity.OPTIONAL, Sorts.K());
        addUnit("OptCell", KLabel(".OptCell"));
        addUnit("ThreadCell", KLabel(".ThreadCellBag"));
        addConcat("ThreadCell", KLabel("_ThreadCellBag_"));
    }};
    LabelInfo labelInfo = new LabelInfo() {{
        addLabel("TopCell", "<top>");
        addLabel("ThreadCell", "<t>");
        addLabel("ThreadCellFragment", "<t>-fragment");
        addLabel("ThreadCellBag", ".ThreadCellBag");
        addLabel("KCell", "<k>");
        addLabel("KCellOpt", "noKCell");
        addLabel("EnvCell", "<env>");
        addLabel("OptCell", "<opt>");
        addLabel("OptCell", ".OptCell");
    }};

    @Test
    public void testSimpleSplitting() {
        KVariable Y = KVariable("Y", Att().add(Attribute.SORT_KEY, "OptCell"));
        K term = KRewrite(cell("<t>", cell("<env>"), KVariable("X"), Y), KVariable("X"));
        K expected = KRewrite(cell("<t>", KVariable("X"), cell("<env>"), Y), cell("<t>-fragment", KVariable("X"), app("noEnvCell"), app(".OptCell")));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    /**
     * Ensure that a variable does not become a cell fragment if it is annotated
     * with a single-cell sort.
     */
    @Test
    public void testSortedVar() {
        KVariable Y = KVariable("Y", Att().add(Attribute.SORT_KEY, "OptCell"));
        K term = KRewrite(cell("<t>", cell("<env>"), KVariable("X"), Y), Y);
        K expected = KRewrite(cell("<t>", KVariable("X"), cell("<env>"), Y), Y);
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    @Test
    public void testUselessVariable() {
        K term = cell("<t>", cell("<env>"), cell("<k>"), cell("<opt>"), KVariable("X"));
        K expected = cell("<t>", cell("<k>"), cell("<env>"), cell("<opt>"));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    @Test
    public void testMultipleSplit() {
        K term = KRewrite(cell("<t>", KVariable("X")), KVariable("Y"));
        K expected = KRewrite(cell("<t>", KVariable("_0"), KVariable("_1"), KVariable("_2")), KVariable("Y"));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    @Test
    public void testAddOptCell() {
        K term = cell("<t>", KVariable("X"), KRewrite(cells(), cell("<opt>")));
        K expected = cell("<t>", KVariable("_0"), KVariable("_1"), KRewrite(KApply(KLabel(".OptCell")), cell("<opt>")));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    @Test
    public void testRemoveOptCell() {
        K term = cell("<t>", KVariable("X"), KRewrite(cell("<opt>"), cells()));
        K expected = cell("<t>", KVariable("_0"), KVariable("_1"), KRewrite(cell("<opt>"), KApply(KLabel(".OptCell"))));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    @Test
    public void testAddStarCell() {
        K term = cell("<top>", KRewrite(cells(), cell("<t>", KVariable("X"))));
        K expected = cell("<top>", KRewrite(KApply(KLabel(".ThreadCellBag")), cell("<t>", KVariable("_0"), KVariable("_1"), KVariable("_2"))));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    @Test
    public void testRemoveStarCell() {
        K term = cell("<top>", KRewrite(cell("<t>", KVariable("X")), cells()));
        K expected = cell("<top>", KRewrite(cell("<t>", KVariable("_0"), KVariable("_1"), KVariable("_2")), KApply(KLabel(".ThreadCellBag"))));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }


    @Test
    public void testConcatStarCell() {
        K term = cell("<top>", KRewrite(KVariable("Y"), cells(KVariable("Y"), cell("<t>", KVariable("X")))));
        K expected = cell("<top>", KRewrite(KVariable("Y"), KApply(KLabel("_ThreadCellBag_"), KVariable("Y"), cell("<t>", KVariable("_0"), KVariable("_1"), KVariable("_2")))));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    @Test
    public void testConcatStarCellEmptyl() {
        K term = cell("<top>", KRewrite(KVariable("Y"), cells()));
        K expected = cell("<top>", KRewrite(KVariable("Y"), KApply(KLabel(".ThreadCellBag"))));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    @Test
    public void testCellFragmentCapture1() {
        K term = cell("<top>", cell("<t>", KVariable("F")),
                cell("<t>", cell("<k>", KRewrite(KSequence(KVariable("Rest")), KSequence(KVariable("F"), KVariable("Rest")))), KVariable("F2")));
        K expected = cell("<top>", app("_ThreadCellBag_",
                cell("<t>", KVariable("_0"), KVariable("_1"), KVariable("_2")),
                cell("<t>", cell("<k>", KRewrite(KSequence(KVariable("Rest")),
                        KSequence(cell("<t>-fragment", KVariable("_0"), KVariable("_1"), KVariable("_2")), KVariable("Rest")))),
                        KVariable("_3"),
                        KVariable("_4"))));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    @Test
    public void testCellFragmentCapture2() {
        K term = cell("<top>", cell("<t>", cell("<k>", KVariable("K")), KVariable("F")),
                cell("<t>", cell("<k>", KRewrite(KSequence(KVariable("Rest")), KSequence(KVariable("F"), KVariable("Rest")))), KVariable("F2")));
        K expected = cell("<top>", app("_ThreadCellBag_",
                cell("<t>", cell("<k>", KVariable("K")), KVariable("_0"), KVariable("_1")),
                cell("<t>", cell("<k>", KRewrite(KSequence(KVariable("Rest")),
                        KSequence(cell("<t>-fragment", app("noKCell"), KVariable("_0"), KVariable("_1")), KVariable("Rest")))),
                        KVariable("_2"),
                        KVariable("_3"))));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    @Test
    public void testCellFragmentCapture3() {
        K term = cell("<top>", cell("<t>", cell("<opt>", KVariable("O")), KVariable("F")),
                cell("<t>", cell("<k>", KRewrite(KSequence(KVariable("Rest")), KSequence(KVariable("F"), KVariable("Rest")))), KVariable("F2")));
        K expected = cell("<top>", app("_ThreadCellBag_",
                cell("<t>", KVariable("_0"), KVariable("_1"), cell("<opt>", KVariable("O"))),
                cell("<t>", cell("<k>", KRewrite(KSequence(KVariable("Rest")),
                        KSequence(cell("<t>-fragment", KVariable("_0"), KVariable("_1"), app(".OptCell")), KVariable("Rest")))),
                        KVariable("_2"),
                        KVariable("_3"))));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    @Test
    public void testCellFragmentCapture4() {
        K term = cell("<top>", cell("<t>", cell("<env>", KVariable("E")), KVariable("F")),
                cell("<t>", cell("<k>", KRewrite(KSequence(KVariable("Rest")), KSequence(KVariable("F"), KVariable("Rest")))), KVariable("F2")));
        K expected = cell("<top>", app("_ThreadCellBag_",
                cell("<t>", KVariable("_0"), cell("<env>", KVariable("E")), KVariable("_1")),
                cell("<t>", cell("<k>", KRewrite(KSequence(KVariable("Rest")),
                        KSequence(cell("<t>-fragment", KVariable("_0"), app("noEnvCell"), KVariable("_1")), KVariable("Rest")))),
                        KVariable("_2"),
                        KVariable("_3"))));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    @Test
    public void testFragmentFragmentCapture() {
        K term = KRewrite(cell("<t>-fragment", app("noKCell"), KVariable("F")), KVariable("F"));
        K expected = KRewrite(cell("<t>-fragment", app("noKCell"), KVariable("_0"), KVariable("_1")),
                cell("<t>-fragment", app("noKCell"), KVariable("_0"), KVariable("_1")));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    @Test
    public void testExplicitFragment1() {
        K term = cell("<top>", cell("<t>",
                cell("<k>", KRewrite(KVariable("Rest"),
                        KSequence(app("<t>-fragment", KVariable("X")), KVariable("Rest")))),
                KVariable("X")));
        K expected = cell("<top>", cell("<t>",
                cell("<k>", KRewrite(KVariable("Rest"),
                        KSequence(app("<t>-fragment", app("noKCell"), KVariable("_0"), KVariable("_1")), KVariable("Rest")))),
                KVariable("_0"),
                KVariable("_1")
        ));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    // Assemble a fragment from variables explicitly labeled with cell sorts
    @Test
    public void testBuildFragment1() {
        KVariable E = KVariable("E", Att().add(Attribute.SORT_KEY, "EnvCell"));
        KVariable K = KVariable("K", Att().add(Attribute.SORT_KEY, "KCell"));
        K term = cell("<t>-fragment", app("#cells", E, K));
        K expected = cell("<t>-fragment", K, E, app(".OptCell"));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    // Assemble a fragment from variables labeled like arguments of the cell fragment label
    @Test
    public void testBuildFragment2() {
        KVariable E = KVariable("E", Att().add(Attribute.SORT_KEY, "EnvCellOpt"));
        KVariable K = KVariable("K", Att().add(Attribute.SORT_KEY, "KCellOpt"));
        K term = cell("<t>-fragment", app("#cells", E, K));
        K expected = cell("<t>-fragment", K, E, app(".OptCell"));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    // Assemble a fragment from literal cells
    @Test
    public void testBuildFragment3() {
        K term = cell("<t>-fragment", app("#cells", app(".OptCell"), cell("<k>", KSequence())));
        K expected = cell("<t>-fragment", cell("<k>", KSequence()), app("noEnvCell"), app(".OptCell"));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    // Assemble a fragment from a cell fragment variable capturing part of a cell,
    // plus some an explicitly-supplied cell.
    @Test
    public void testBuildFragment4() {
        K term = cell("<t>", cell("<k>", KRewrite(
                app("capture"),
                app("captured", app("<t>-fragment", app("#cells", app("<k>", KSequence()), KVariable("Ctx"))))
        )), KVariable("Ctx"));
        K expected = cell("<t>", cell("<k>", KRewrite(
                app("capture"),
                app("captured", app("<t>-fragment", app("<k>", KSequence()), KVariable("_0"), KVariable("_1")))
        )), KVariable("_0"), KVariable("_1"));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    // like {@code testBuildFragment4}, but with a KCell variable
    @Test
    public void testBuildFragment5() {
        KVariable K = KVariable("K", Att().add(Attribute.SORT_KEY, "KCellOpt"));
        K term = cell("<t>", cell("<k>", KRewrite(
                app("capture", K),
                app("captured", app("<t>-fragment", app("#cells", K, KVariable("Ctx"))))
        )), KVariable("Ctx"));
        K expected = cell("<t>", cell("<k>", KRewrite(
                app("capture", K),
                app("captured", app("<t>-fragment", K, KVariable("_0"), KVariable("_1")))
        )), KVariable("_0"), KVariable("_1"));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    ConfigurationInfo bagCfgInfo = new TestConfiguration() {{
        addCell(null, "TopCell", "<top>");
        addCell("TopCell", "ThreadCell", "<t>", Multiplicity.STAR, Sorts.K());
        addCell("TopCell", "ExtraCell", "<extra>");
        addUnit("ThreadCell", KLabel(".ThreadCellBag"));
        addConcat("ThreadCell", KLabel("_ThreadCellBag_"));
    }};
    LabelInfo bagLabelInfo = new LabelInfo() {{
        addLabel("TopCell", "<top>");
        addLabel("ThreadCell", "<t>");
        addLabel("ExtraCell", "<extra>");
        addLabel("K", "restore");
        addLabel("ThreadCellBag", "_ThreadCellBag_");
        addLabel("ThreadCellBag", ".ThreadCellBag");
    }};

    @Test
    public void testFragmentBag() {
        K term = cell("<top>", KVariable("F"), cell("<t>", KRewrite(KVariable("Rest"), KSequence(KVariable("F"), KVariable("Rest")))));
        K expected = cell("<top>",
                app("_ThreadCellBag_", KVariable("_0"),
                        cell("<t>", KRewrite(KVariable("Rest"),
                                KSequence(app("<top>-fragment", KVariable("_0"), KVariable("_1")), KVariable("Rest"))))),
                KVariable("_1"));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(bagCfgInfo, bagLabelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    @Test
    public void testFragmentRewrite() {
        K term = cell("<top>",
                cell("<t>", KRewrite(app("restore", KVariable("Ctx")),
                        KVariable("Result"))),
                KRewrite(cells(KVariable("_1"), cell("<t>", KVariable("Result"))),
                        KVariable("Ctx")));
        K expected = cell("<top>",
                app("_ThreadCellBag_",
                        cell("<t>", KRewrite(
                                app("restore", app("<top>-fragment", KVariable("_0"), KVariable("_2"))),
                                KVariable("Result"))),
                        KRewrite(app("_ThreadCellBag_", KVariable("_3"),
                                cell("<t>", KVariable("Result"))),
                                KVariable("_0"))),
                KRewrite(KVariable("_4"), KVariable("_2")));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(bagCfgInfo, bagLabelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    /**
     * When a cell fragment variable occurs as an argument to the appropriate cell fragment sort predict, we
     * specialize the expansion by splitting it into a conjunction of individual sort predicate tests on
     * the variables the cell fragment variable splits into, rather than just using the generic replacement term.
     * This is a very special case of statically simplifying predicate applications.
     */
    @Test
    public void testPredicateExpansion() {
        Rule term = new Rule(KRewrite(cell("<t>", cell("<env>"), KVariable("X"), KVariable("Y", Att().add(Attribute.SORT_KEY, "OptCell"))), KVariable("X"))
                , app("isThreadCellFragment", KVariable("X"))
                , BooleanUtils.TRUE
                , Att());
        K expectedBody = KRewrite(cell("<t>", KVariable("X"), cell("<env>"), KVariable("Y", Att().add(Attribute.SORT_KEY, "OptCell"))),
                cell("<t>-fragment", KVariable("X"), app("noEnvCell"), app(".OptCell")));
        Rule expected = new Rule(expectedBody
                , BooleanUtils.and(BooleanUtils.TRUE, app("isKCell", KVariable("X")))
                , BooleanUtils.TRUE, Att());
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    /**
     * Ensure that the splitting in {@linkplain #testPredicateExpansion()} does not happen
     * in a term which applies the sort predicate for a <em>different</em> cell fragment sort
     * to a cell fragment variable.
     */
    @Test
    public void testUnrelatedPredicate() {
        Rule term = new Rule(KRewrite(cell("<t>", cell("<env>"), KVariable("X"), KVariable("Y", Att().add(Attribute.SORT_KEY, "OptCell"))), KVariable("X"))
                , app("isTopCellFragment", KVariable("X"))
                , BooleanUtils.TRUE
                , Att());
        K replacement = app("<t>-fragment", KVariable("X"), app("noEnvCell"), app(".OptCell"));
        K expectedBody = KRewrite(cell("<t>", KVariable("X"), cell("<env>"), KVariable("Y", Att().add(Attribute.SORT_KEY, "OptCell"))), replacement);
        Rule expected = new Rule(expectedBody
                , app("isTopCellFragment", replacement)
                , BooleanUtils.TRUE, Att());
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(cfgInfo, labelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    /**
     * Test that splitting works properly if an item under a parent cell is
     * already a collection of multiplicity * cells joined by the proper label.
     */
    @Test
    public void testMultipleCells() {
        KVariable T1 = KVariable("T1", Att().add(Attribute.SORT_KEY, "ThreadCell"));
        KVariable T2 = KVariable("T2", Att().add(Attribute.SORT_KEY, "ThreadCell"));
        K term = cell("<top>",
                KVariable("F"),
                cell("<t>", KVariable("T")),
                app("_ThreadCellBag_", T1, T2));
        K expected = cell("<top>",
                app("_ThreadCellBag_",
                        app("_ThreadCellBag_", KVariable("_0"), cell("<t>", KVariable("T"))),
                        app("_ThreadCellBag_", T1, T2)),
                KVariable("_1"));
        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        Assert.assertEquals(expected, new SortCells(bagCfgInfo, bagLabelInfo, kem).sortCells(term));
        Assert.assertEquals(0, kem.getExceptions().size());
    }

    KApply app(String name, K... ks) {
        return KApply(KLabel(name), ks);
    }

    KApply cell(String name, K... ks) {
        return KApply(KLabel(name), ks);
    }

    KApply cells(K... ks) {
        return KApply(KLabel(KLabels.CELLS), ks);
    }
}
