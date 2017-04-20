// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.parser.concrete2kore.disambiguation;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import org.kframework.attributes.Source;
import org.kframework.definition.Definition;
import org.kframework.definition.Module;
import org.kframework.legacykore.K;
import org.kframework.legacykore.KApply;
import org.kframework.legacykore.KLabel;
import org.kframework.parser.TreeNodesToKORE;
import org.kframework.parser.concrete2kore.ParseInModule;
import org.kframework.parser.concrete2kore.generator.RuleGrammarGenerator;
import org.kframework.utils.errorsystem.ParseFailedException;
import scala.Tuple2;
import scala.util.Either;

import java.util.Set;

import static org.kframework.legacykore.KORE.*;

public class AddEmptyListsTest {
    private ParseInModule parser;

    @Rule
    public TestName testName = new TestName();

    @Before
    public void setUp() throws Exception {
        /*
        FileUtil files = FileUtil.testFileUtil();
        File definitionFile = new File(Kompile.BUILTIN_DIRECTORY.toString() + "/kast.k");
        String baseKText = files.loadFromWorkingDirectory(definitionFile.getPath());
         */
        String baseKText = "require \"domains.k\"\n";
        Definition baseK = org.kframework.DefinitionParser.from(baseKText + DEF, "TEST");
        Module test = baseK.getModule("TEST").get();
        parser = RuleGrammarGenerator.getCombinedGrammar(RuleGrammarGenerator.getRuleGrammar(test, s -> baseK.getModule(s).get()), true);
    }

    private void parseTerm(String term, String sort, K expected) {
        parseTerm(term, sort, expected, 0);
    }

    private void parseTerm(String term, String sort, K expected, int expectWarnings) {
        String source = "AddEmpytListsTest." + testName.getMethodName();
        final Tuple2<Either<Set<ParseFailedException>, K>, Set<ParseFailedException>> parseResult
                = parser.parseString(term, Sort(sort), new Source(source));
        if (parseResult._1().isLeft()) {
            Assert.assertTrue("Unexpected parse errors" + parseResult._1().left().get(), false);
        }
        K actual = TreeNodesToKORE.down(parseResult._1().right().get());
        Assert.assertEquals(expected, actual);
        if (parseResult._2().size() != expectWarnings) {
            Assert.assertTrue("Unexpected parse warnings" + parseResult._2(), false);
        }
    }

    private static final String DEF =
            "module TEST\n" +
                    "syntax A ::= \"a\" [klabel(\"alabel\")]\n" +
                    "syntax B ::= \"b\" [klabel(\"blabel\")]\n" +
                    "syntax A ::= B\n" +
                    "syntax As ::= List{A,\",\"}\n" +
                    "syntax Bs ::= List{B,\",\"}\n" +
                    "syntax As ::= Bs\n" + // TODO: no longer needed. Fixed in #1891 in master branch
                    "syntax Func ::= f(As) | g(A) | h(Bs)" +
                    "endmodule\n";

    public static final KApply NIL = KApply(KLabel(".List{\"_,_\"}"));
    public static final KLabel CONS = KLabel("_,_");
    public static final KApply A = KApply(KLabel("alabel"));
    public static final KApply B = KApply(KLabel("blabel"));
    public static final KLabel F = KLabel("f");
    public static final KLabel G = KLabel("g");
    public static final KLabel H = KLabel("h");
    public static final KLabel CAST_A = KLabel("#SemanticCastToA@TEST");
    public static final KLabel CAST_B = KLabel("#SemanticCastToB@TEST");
    public static final KLabel CAST_AS = KLabel("#SemanticCastToAs@TEST");
    public static final KLabel CAST_BS = KLabel("#SemanticCastToBs@TEST");

    @Test
    public void testEmptyList1() {
        parseTerm(".As", "As", NIL);
    }

    @Ignore("The API of AddEmptyLists needs to change for this to be possible")
    @Test
    public void testItem() {
        parseTerm("a", "As", KApply(CONS, A, NIL));
    }

    @Test
    public void testConcreteTop() {
        parseTerm(".As", "As", NIL);
        parseTerm("a,a", "As", KApply(CONS, A, KApply(CONS, A, NIL)));
        parseTerm("a,.As", "As", KApply(CONS, A, NIL));
        parseTerm("a,b", "As", KApply(CONS, A, KApply(CONS, B, NIL)));
        parseTerm("b,.Bs", "As", KApply(CONS, B, NIL));
        parseTerm("b,b", "As", KApply(CONS, B, KApply(CONS, B, NIL)));
    }

    @Test
    public void testConcreteArgument() {
        parseTerm("f(.As)", "Func", KApply(F, NIL));
        parseTerm("f(a)", "Func", KApply(F, KApply(CONS, A, NIL)));
        parseTerm("f(a,a)", "Func", KApply(F, KApply(CONS, A, KApply(CONS, A, NIL))));
        parseTerm("f(a,.As)", "Func", KApply(F, KApply(CONS, A, NIL)));
        parseTerm("f(a,b)", "Func", KApply(F, KApply(CONS, A, KApply(CONS, B, NIL))));
        parseTerm("f(b,.Bs)", "Func", KApply(F, KApply(CONS, B, NIL)));
        parseTerm("f(b,b)", "Func", KApply(F, KApply(CONS, B, KApply(CONS, B, NIL))));
    }

    @Ignore("BUG: need to also propagate correct sorts to arguments of labeled application")
    @Test
    public void testLabeledFunSingleItem() {
        parseTerm("`f`(a)", "K", KApply(F, KApply(CONS, A, NIL)));
    }

    @Test
    public void testLabedFunConcreteArgument() {
        parseTerm("`f`(.As)", "K", KApply(F, NIL));
        parseTerm("`f`((a,a))", "K", KApply(F, KApply(CONS, A, KApply(CONS, A, NIL))));
        parseTerm("`f`((a,.As))", "K", KApply(F, KApply(CONS, A, NIL)));
        parseTerm("`f`((a,b))", "K", KApply(F, KApply(CONS, A, KApply(CONS, B, NIL))));
        parseTerm("`f`((b,.Bs))", "K", KApply(F, KApply(CONS, B, NIL)));
        parseTerm("`f`((b,b))", "K", KApply(F, KApply(CONS, B, KApply(CONS, B, NIL))));
    }

    @Test
    public void testAnnVar() {
        parseTerm("V:As", "K", KApply(CAST_AS, KVariable("V")));
    }

    @Test
    public void testArgumentLabeledCons() {
        parseTerm("f(`_,_`(a,.As))", "Func", KApply(F, KApply(CONS, A, NIL)));
    }

    @Test
    public void testArgumentLabeledNil() {
        parseTerm("f(`.List{\"_,_\"}`(.KList))", "K", KApply(F, NIL));
    }

    @Test
    public void testArgumentLabeledConsSub1() {
        parseTerm("h(`_,_`(b,.Bs))", "Func", KApply(H, KApply(CONS, B, NIL)));
    }

    @Test
    public void testArgumentLabeledConsSub2() {
        // gets a warning because the argument of sort As does not fit.n
        parseTerm("h(`_,_`(a,.As))", "Func", KApply(H, KApply(CONS, A, NIL)), 1);
    }

    @Test
    public void testArgumentLabeledNilSub1() {
        parseTerm("h(`.List{\"_,_\"}`(.KList))", "K", KApply(H, NIL));
    }

    @Test
    public void testArgumentInferredListVar() {
        // 1 warning from inference
        parseTerm("f(V)", "Func", KApply(F, KApply(CAST_AS, KVariable("V"))), 1);
    }

    @Test
    public void testArgumentAnnListVar() {
        parseTerm("f(V:As)", "Func", KApply(F, KApply(CAST_AS, KVariable("V"))));
    }

    @Test
    public void testArgumentAnnSubListVar() {
        parseTerm("f(V:Bs)", "Func", KApply(F, KApply(CAST_BS, KVariable("V"))));
    }

    @Test
    public void testArgumentInferredItemVar() {
        // 1 warning from inference
        parseTerm("f(V)~>g(V)", "Func",
                KSequence(KApply(F, KApply(CONS, KApply(CAST_A, KVariable("V")), NIL)),
                        KApply(G, KApply(CAST_A, KVariable("V")))), 1);
    }

    @Test
    public void testArgumentAnnItemVar() {
        parseTerm("f(V:A)", "Func",
                KApply(F, KApply(CONS, KApply(CAST_A, KVariable("V")), NIL)));
    }

    @Test
    public void testArgumentAnnSubItemVar() {
        parseTerm("f(V:B)", "Func",
                KApply(F, KApply(CONS, KApply(CAST_B, KVariable("V")), NIL)));
    }
}
