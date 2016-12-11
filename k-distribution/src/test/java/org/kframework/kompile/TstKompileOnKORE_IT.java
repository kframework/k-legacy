package org.kframework.krun;

import com.beust.jcommander.JCommander;
import org.apache.commons.lang3.exception.ExceptionContext;
import org.apache.commons.lang3.time.StopWatch;
import org.junit.Before;
import org.junit.Test;
import org.kframework.AbstractTest;
import org.kframework.Kapi;
import org.kframework.RewriterResult;
import org.kframework.backend.java.symbolic.JavaBackend;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kompile.Kompile;
import org.kframework.kompile.KompileOptions;
import org.kframework.kore.KORE;
import org.kframework.kore.compile.Backend;
import org.kframework.main.GlobalOptions;
import org.kframework.main.Tool;
import org.kframework.utils.KoreUtils;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.inject.CommonModule;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URISyntaxException;

import static org.junit.Assert.*;

/**
 * Created by Youshan on 11/1/14.
 */
public class TstKompileOnKORE_IT extends AbstractTest {

    /* *
     * kompileEmptyModuleTest simulates the command for bug#2219
     * `-kompile --no-prelude --debug k-distribution/src/test/resources/compiler-tests/empty-module.k`
     * The empty-module.k is created with no imports, syntax, rules, and configuration in it.
     * Before bug2219 is fixed, an NoSuchElementException is thrown.
     * After the bug is fixed, a compiledDefinition is expected to be generated with an exit code 0.
     */
    @Test
    public void kompileEmptyModuleTest() {
        // set --no-prelude option
        KompileOptions kompileOptions = new KompileOptions();
        kompileOptions.outerParsing.noPrelude = true;
        // set --debug option
        GlobalOptions globalOptions = new GlobalOptions();
        globalOptions.debug = true;
        // set definitionFile with its' module name
        File defintionFile = new File("src/test/resources/compiler-tests/empty-module.k");
        String mainModuleName = "EMPTY-MODULE";
        String mainProgramsModuleName = "EMPTY-MODULE";


        // kompile definition
        Kompile kompile = new Kompile(kompileOptions, FileUtil.testFileUtil(), kem, false);
        CompiledDefinition compiledDef = kompile.run(defintionFile, mainModuleName, mainProgramsModuleName,
                new JavaBackend(kem, FileUtil.testFileUtil(), globalOptions, kompileOptions).steps());

        assertEquals(compiledDef.topCellInitializer, KORE.KLabel(""));
    }

}
