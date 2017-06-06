package org.kframework.backend.skala;

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

import javax.swing.text.html.Option;
import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;

import static org.junit.Assert.*;


public class BasicOnSkalaTest {
    @Test
    public void basicOnSkalaTest1() {
        String fileName = "src/test/resources/basic/basic.k";
        String program = "1 + 2";
        Source source = Source.apply("from test");
        String mainModuleName = "BASIC";

        KExceptionManager kem = new KExceptionManager(new GlobalOptions());
        File definitionFile = new File(fileName);
        KompileOptions kompileOptions = new KompileOptions();
        kompileOptions.backend = Backends.SKALA;
        GlobalOptions globalOptions = new GlobalOptions();
        globalOptions.debug = true;
        globalOptions.warnings = GlobalOptions.Warnings.ALL;

        Kompile kompile = new Kompile(kompileOptions, FileUtil.testFileUtil(), kem, false);

        CompiledDefinition compiledDef = kompile.run(definitionFile, mainModuleName, mainModuleName, new SkalaKompile(kompileOptions, kem).steps());

        BiFunction<String, Source, K> programParser = compiledDef.getProgramParser(kem);

        K parsed = programParser.apply(program, source);

        Map<KToken, K> map = new HashMap<>();

        map.put(KORE.KToken("$PGM", Sorts.KConfigVar()), parsed);

        KApply input = KORE.KApply(compiledDef.topCellInitializer, map.entrySet().stream().map(e -> KORE.KApply(KORE.KLabel("_|->_"), e.getKey(), e.getValue())).reduce(KORE.KApply(KORE.KLabel(".Map")), (a, b) -> KORE.KApply(KORE.KLabel("_Map_"), a, b)));

        Definition definition = KoreToMini.apply(compiledDef.kompiledDefinition);

        SkalaRewriter skalaBackendRewriter = new SkalaRewriter(compiledDef.executionModule(), definition);

        K kResult = skalaBackendRewriter.execute(input, Optional.empty()).k();

        Module unparsingModule = compiledDef.getExtensionModule(compiledDef.languageParsingModule());

        String actual = KOREToTreeNodes.toString(new AddBrackets(unparsingModule).addBrackets((org.kframework.parser.ProductionReference) KOREToTreeNodes.apply(KOREToTreeNodes.up(unparsingModule, kResult), unparsingModule)));

        assertEquals("Execution failed", "<T> <k> 3 </k> </T>", actual);

    }
}
