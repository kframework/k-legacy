// Copyright (c) 2015-2016 K Team. All Rights Reserved.

package org.kframework.frontend.compile;

import org.junit.Test;
import org.junit.rules.TestName;
import org.kframework.attributes.Source;
import org.kframework.backend.Backends;
import org.kframework.builtin.Sorts;
import org.kframework.definition.Module;
import org.kframework.frontend.K;
import org.kframework.frontend.KApply;
import org.kframework.frontend.KORE;
import org.kframework.frontend.KToken;
import org.kframework.kale.KaleBackend;
//import org.kframework.kale.KaleRewriter;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kompile.Kompile;
import org.kframework.kompile.KompileOptions;
import org.kframework.main.GlobalOptions;
import org.kframework.parser.ProductionReference;
import org.kframework.unparser.AddBrackets;
import org.kframework.unparser.KOREToTreeNodes;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;

import static org.junit.Assert.*;
import static org.kframework.frontend.KORE.*;

public class IMPonKale {

    @org.junit.Rule
    public TestName name = new TestName();

    protected File testResource(String baseName) throws URISyntaxException {
        return new File(baseName);
    }

    @Test
    public void simple() throws IOException, URISyntaxException {
//        String fileName = "tutorial/1_k/2_imp/lesson_4/imp.k";
//        String program = "int n, sum;\n" +
//                "n = 10;\n" +
//                "sum = 0;\n" +
//                "while (!(n <= 0)) {\n" +
//                "  sum = sum + n;\n" +
//                "  n = n + -1;\n" +
//                "}";
//        Source source = Source.apply("from test");
//        String mainModuleName = "IMP";
//
//        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
//        File definitionFile = testResource(fileName);
//        KompileOptions kompileOptions = new KompileOptions();
//        kompileOptions.backend = Backends.KALE;
//        GlobalOptions globalOptions = new GlobalOptions();
//        globalOptions.debug = true;
//        globalOptions.warnings = GlobalOptions.Warnings.ALL;
//
//        Kompile kompile = new Kompile(kompileOptions, FileUtil.testFileUtil(), kem, false);
//
//        CompiledDefinition compiledDef = kompile.run(definitionFile, mainModuleName, mainModuleName,  new KaleBackend(kompileOptions, kem).steps());
//
//        BiFunction<String, Source, K> programParser = compiledDef.getProgramParser(kem);
//
//        K parsed = programParser.apply(program, source);
//
//        Map<KToken, K> map = new HashMap<>();
//        map.put(KORE.KToken("$PGM", Sorts.KConfigVar()), parsed);

//        KApply input = KORE.KApply(compiledDef.topCellInitializer, map.entrySet().stream().map(e -> KApply(KLabel("_|->_"), e.getKey(), e.getValue())).reduce(KApply(KLabel(".Map")), (a, b) -> KApply(KLabel("_Map_"), a, b)));

//        KaleRewriter kaleRewriter = new KaleRewriter(compiledDef.executionModule());

//        K kResult = kaleRewriter.execute(input, Optional.<Integer>empty()).k();

//        Module unparsingModule = compiledDef.getExtensionModule(compiledDef.languageParsingModule());

//        String actual = KOREToTreeNodes.toString(new AddBrackets(unparsingModule).addBrackets((ProductionReference) KOREToTreeNodes.apply(KOREToTreeNodes.up(unparsingModule, kResult), unparsingModule)));

//        assertEquals("Execution failed", "<T> <k> . </k> <state> sum |-> 55 n |-> 0 </state> </T>", actual);
    }
}
