package org.kframework.kompile;

import org.junit.Before;
import org.junit.Test;
import org.kframework.AbstractTest;
import org.kframework.backend.java.symbolic.JavaBackend;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.file.FileUtil;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;

import static org.junit.Assert.*;

public class TstKompileOnKORE_IT extends AbstractTest {

    private KompileOptions kompileOptions;
    private GlobalOptions globalOptions;

    // for kompile
    private Kompile kompile;
    private File definitionFile;
    private File tmpDir;
    private File definitionDir;
    private String mainModuleName;
    private String mainProgramsModuleName;
    private CompiledDefinition compiledDef;
    private FileUtil files;



    @Before
    public void setup() throws URISyntaxException, IOException {
        kompileOptions = new KompileOptions();
        globalOptions = new GlobalOptions();
        // set definitionFile with its' module name
        tmpDir = new File("src/test/resources/compiler-tests/tmpDir");
        definitionDir = new File("src/test/resources/compiler-tests/");
        definitionFile = new File("src/test/resources/compiler-tests/empty-module.k");
        mainModuleName = "EMPTY-MODULE";
        mainProgramsModuleName = "EMPTY-MODULE";
        files = new FileUtil(tmpDir, definitionDir, definitionDir, definitionDir, globalOptions, System.getenv());
    }

     /* *
     * kompileEmptyModuleTest simulates the command for bug#2219
     * `-kompile --no-prelude --debug k-distribution/src/test/resources/compiler-tests/empty-module.k`
     * The empty-module.k is created with no imports and syntax but possibly rules and configurations in it.
     * Before bug2219 is fixed, an NoSuchElementException is thrown in ModuleTransformerException class.
     * After the bug is fixed, there should be no such exceptions.
     */

    @Test
    public void kompileEmptyModuleTest() {
        // set --no-prelude option
        kompileOptions.outerParsing.notAutoImportDomain = true;

        // kompile definition
        kompile = new Kompile(kompileOptions, files, kem, false);
        String exceptionCauseClass = null;
        try {
            compiledDef = kompile.run(definitionFile, mainModuleName, mainProgramsModuleName,
                    new JavaBackend(kem, files, globalOptions, kompileOptions).steps());
        } catch (Throwable throwable){
            exceptionCauseClass = throwable.getClass().getName();
            throwable.printStackTrace(); // set --debug option
        }
        // Make sure there is no exception
        assertNull(exceptionCauseClass);
    }

}
