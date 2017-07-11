package org.kframework.backend.skala;


import com.sun.tools.javac.util.List;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.kframework.attributes.Source;
import org.kframework.backend.Backends;
import org.kframework.builtin.Sorts;
import org.kframework.definition.Module;
import org.kframework.frontend.K;
import org.kframework.frontend.KApply;
import org.kframework.frontend.KORE;
import org.kframework.frontend.KToken;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kompile.Kompile;
import org.kframework.kompile.KompileOptions;
import org.kframework.kore.Definition;
import org.kframework.main.GlobalOptions;
import org.kframework.minikore.converters.KoreToMini;
import org.kframework.unparser.AddBrackets;
import org.kframework.unparser.KOREToTreeNodes;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;

import static org.junit.Assert.*;

public class PlutusOnSkalaTest {

    private static CompiledDefinition compiledDef;
    private static String resources;
    private static KExceptionManager kem;
    private static SkalaRewriter skalaBackendRewriter;
    private static Module unparsingModule;
    private static BiFunction<String, Source, K> programParser;

    static long timeBefore = System.nanoTime();

    public static void logTime(String what) {
        System.out.println(what + ": " + ((System.nanoTime() - timeBefore) / 1000000) + "ms");
        timeBefore = System.nanoTime();
    }

    @BeforeClass
    public static void kompileSetup() {
        logTime("start");
        resources = "src/test/resources/plutus/";
        File definitionFile = new File(resources + "plutus-core.k");
        String mainModuleName = "PLUTUS-CORE";
        KompileOptions kompileOptions = new KompileOptions();
        kompileOptions.backend = Backends.SKALA;
        GlobalOptions globalOptions = new GlobalOptions();
        globalOptions.debug = true;
        globalOptions.warnings = GlobalOptions.Warnings.ALL;
        kem = new KExceptionManager(globalOptions);
        Kompile kompile = new Kompile(kompileOptions, FileUtil.testFileUtil(), kem, false);
        compiledDef = kompile.run(definitionFile, mainModuleName, mainModuleName, new SkalaKompile(kompileOptions, kem).steps());
        logTime("kompile");
        Definition definition = KoreToMini.apply(compiledDef.kompiledDefinition);
        logTime("frontend kore to kore");
        skalaBackendRewriter = new SkalaRewriter(compiledDef.executionModule(), definition);
        logTime("make Scala Rewriter");
        unparsingModule = compiledDef.getExtensionModule(compiledDef.languageParsingModule());
        programParser = compiledDef.getProgramParser(kem);
        logTime("parse-unparse stuff");

        // warmup
        K input = getParsedProgram("empty.plcore");
        K kResult = skalaBackendRewriter.execute(input, Optional.empty()).k();
        String actual = unparseResult(kResult);
    }

    private static K getParsedProgram(String pgmName) {
        String program = FileUtil.load(new File(resources + pgmName));
        Source source = Source.apply("from test");
        K parsed = programParser.apply(program, source);
        Map<KToken, K> map = new HashMap<>();
        map.put(KORE.KToken("$PGM", Sorts.KConfigVar()), parsed);
        KApply input = KORE.KApply(compiledDef.topCellInitializer, map.entrySet().stream().map(e -> KORE.KApply(KORE.KLabel("_|->_"), KORE.KList(List.of(e.getKey(), e.getValue())))).reduce(KORE.KApply(KORE.KLabel(".Map")), (a, b) -> KORE.KApply(KORE.KLabel("_Map_"), a, b)));
        return input;
    }

    private static String unparseResult(K result) {
        return KOREToTreeNodes.toString(new AddBrackets(unparsingModule).addBrackets((org.kframework.parser.ProductionReference) KOREToTreeNodes.apply(KOREToTreeNodes.up(unparsingModule, result), unparsingModule)));
    }

    @Ignore
    @Test
    public void sumTest() {
        K input = getParsedProgram("empty.plcore");
        K kResult = skalaBackendRewriter.execute(input, Optional.empty()).k();
        String actual = unparseResult(kResult);
        assertEquals("Execution with Skala Backend Failed", "<T> <k> . </k> <state> 'n |-> 0 'sum |-> 55 </state> </T>", actual);
    }
}
