// Copyright (c) 2014-2015 K Team. All Rights Reserved.

package org.kframework.kore.convertors;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.kframework.Collections;
import org.kframework.definition.Module;
import org.kframework.kore.K;
import org.kframework.kore.KApply;
import org.kframework.kore.ScalaSugar;
import org.kframework.kore.Unparse;
import org.kframework.tiny.Constructors;
import org.kframework.tiny.FreeTheory;
import org.kframework.tiny.KApp;
import org.kframework.tiny.KIndex$;
import org.kframework.tiny.Rewriter;
import org.kframework.tiny.Rule;
import org.kframework.tiny.package$;
import scala.collection.immutable.Set;

import static org.kframework.Collections.*;
import static org.kframework.definition.Constructors.*;
import static org.kframework.kore.KORE.*;

import java.io.IOException;


public class TstTinyOnKORE extends BaseTest {


    @Test
    public void kore_imp_tiny() throws IOException {
        sdfTest();
    }

    protected String convert(DefinitionWithContext defWithContext) {
        KILtoKORE kilToKore = new KILtoKORE(defWithContext.context);
        Module moduleWithoutK = kilToKore.apply(defWithContext.definition).getModule("TEST").get();

        Module module = Module("IMP", Set(moduleWithoutK), Set(
                Production(Sort("K"), Seq(NonTerminal(Sort("KSequence"))))
        ), Att());

        Constructors cons = new Constructors(module);
        ScalaSugar sugar = new <org.kframework.kore.K>ScalaSugar(cons);

        K program = KApply("<top>",
                KApply("<k>",
                        KApply("while__",
                                KApply("_<=_", (K) sugar.intToToken(0), (K) sugar.stringToId("n")),
                                KApply("__",
                                        KApply("_=_;",
                                                (K) sugar.stringToId("s"),
                                                KApply("_+_",
                                                        (K) sugar.stringToId("s"),
                                                        (K) sugar.stringToId("n"))),
                                        KApply("_=_;",
                                                (K) sugar.stringToId("n"),
                                                KApply("_+_",
                                                        (K) sugar.stringToId("n"),
                                                        (K) sugar.intToToken(-1)))
                                ))),
                KApply("<state>",
                        KApply("_Map_",
                                KApply("_|->_", (K) sugar.stringToId("n"), (K) sugar.intToToken(10)),
                                KApply("_|->_", (K) sugar.stringToId("s"), (K) sugar.intToToken(0)))
                )
        );

//        program = KApply(KApply("'<top>"),
//                KApply(KApply("'<k>"),
//                        KApply("'_/_", sugar.stringToId("x"), sugar.stringToId("y"))),
//                KApply(KLabel("'<state>"),
//                        KApply(KLabel("'_Map_"),
//                                KApply(KLabel("'_|->_"), sugar.stringToId("x"), sugar.intToToken(10)),
//                                KApply(KLabel("'_|->_"), sugar.stringToId("y"), sugar.intToToken(2)))
//                ));


        System.out.println("module = " + module);

        Rewriter rewriter = new Rewriter(module, KIndex$.MODULE$);

//        long l = System.nanoTime();
//        Set<K> results = rewriter.rewrite(program, Set());
//        System.out.println("time = " + (System.nanoTime() - l) / 1000000);
//
//        return stream(results).map(r -> r.toString()).collect(Collections.toList()).mkString("\n");

        long l = System.nanoTime();
        K result = rewriter.execute(cons.convert(program));
        System.out.println("time = " + (System.nanoTime() - l) / 1000000);

        return result.toString();
    }

    @Override
    protected String expectedFilePostfix() {
        return "-tiny-expected.txt";
    }
}
