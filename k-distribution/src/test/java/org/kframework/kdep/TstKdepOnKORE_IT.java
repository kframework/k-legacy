package org.kframework.kdep;

import com.beust.jcommander.JCommander;
import com.google.common.collect.ImmutableSet;
import org.junit.Before;
import org.junit.Test;
import org.kframework.main.Tool;
import org.kframework.utils.Stopwatch;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.inject.JCommanderModule;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URISyntaxException;
import java.util.Set;

import static org.junit.Assert.*;

public class TstKdepOnKORE_IT {
    private KDepOptions kDepOptions;
    private KDepFrontEnd frontEnd;

    private File definitionFile;
    private File tmpDir;
    private File definitionDir;
    private FileUtil files;

    private KExceptionManager kem;
    private Stopwatch sw;

    @Before
    public void setup() throws URISyntaxException, IOException {
        kDepOptions = new KDepOptions();
        // set definitionFile with its' module name
        tmpDir = new File("src/test/resources/compiler-tests/tmpDir");
        definitionDir = new File("src/test/resources/compiler-tests/");
        definitionFile = new File("src/test/resources/compiler-tests/empty-module.k");
        files = new FileUtil(tmpDir, definitionDir, definitionDir, definitionDir, kDepOptions.global, System.getenv());
    }

    /* *
     * kdepEmptyModuleTest runs the regression test for
     * `kdep --no-prelude k-distribution/src/test/resources/compiler-tests/empty-module.k`
     * The empty-module.k is created with no imports and syntax but possibly rules and configurations in it.
     * Before bug2219 is fixed:
     * timestamp : \
     *     /Users/Youshan/Documents/427bugfix/k-distribution/src/test/resources/compiler-tests/empty-module.k \
     * After the bug2219 is fixed:
     * timestamp : \
     *     /Users/Youshan/Documents/427bugfix/k-distribution/src/test/resources/compiler-tests/empty-module.k \
     *     /Users/Youshan/Documents/427bugfix/kernel/target/../../k-distribution/include/builtin/kast.k \
     *     /Users/Youshan/Documents/427bugfix/kernel/target/../../k-distribution/include/builtin/domains.k \
     * This tests makes sure that when we set -no-prelude flag, kast.k and domains.k are loaded
     * but not imported.
     */

    private String runKDepAndRedirectOutput() {
        // redirect system stdout to outstream and save in baos
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream outstream = new PrintStream(baos);
        PrintStream old = System.out; // save the previous System.out
        System.setOut(outstream);

        frontEnd.main();
        String output = baos.toString();

        // redirect output to stdout again
        System.setOut(old);
        return output;
    }

    @Test
    public void kdepEmptyModuleTest() {
        // set --no-prelude option
        kDepOptions.outerParsing.notAutoImportDomain = true;
        kem = new KExceptionManager(kDepOptions.global);
        sw = new Stopwatch(kDepOptions.global);
        String[] args = new String[]{"empty-module.k", definitionDir.toString()};

        // parsing options
        Set<Object> options = ImmutableSet.of(kDepOptions);
        Set<Class<?>> experimentalOptions = ImmutableSet.of();
        JCommander jc = JCommanderModule.jcommander(args, Tool.KDEP, options, experimentalOptions, kem, sw);
        JCommanderModule.usage(jc);
        JCommanderModule.experimentalUsage(jc);

        // run kdep by running the main
        frontEnd = new KDepFrontEnd(kDepOptions.outerParsing, kem, kDepOptions.global, sw, files);
        String[] realOutputs = runKDepAndRedirectOutput().split("\n");

        String[] expectedOutputs = new String[]{"timestamp : \\",
            "    /Users/Youshan/Documents/427bugfix/k-distribution/src/test/resources/compiler-tests/empty-module.k \\",
            "    /Users/Youshan/Documents/427bugfix/kernel/target/../../k-distribution/include/builtin/kast.k \\",
            "    /Users/Youshan/Documents/427bugfix/kernel/target/../../k-distribution/include/builtin/domains.k \\"};

        assertArrayEquals(realOutputs, expectedOutputs);
    }
}
