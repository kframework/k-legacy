package org.kframework.krun;
import com.beust.jcommander.JCommander;
import org.junit.Test;
import org.kframework.AbstractTest;
import org.kframework.Kapi;
import org.kframework.RewriterResult;
import org.kframework.backend.java.symbolic.JavaBackend;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kompile.Kompile;
import org.kframework.kompile.KompileOptions;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.file.FileUtil;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;

import static org.junit.Assert.*;
 
public class TstKRunOnKORE_IT extends AbstractTest {
    /* *
     * krunEmptyModuleTest simulates the command for bug#2219
     * `-krun --no-prelude --debug k-distribution/src/test/resources/compiler-tests/empty-module.k --output pretty`
     * The empty-module.k is created with no imports, syntax, rules, and configuration in it.
     * Before bug2219 is fixed, an NoSuchElementException is thrown.
     * After the bug is fixed, an empty output is expected to be generated with an exit code 0.
     */
    @Test
    public void krunEmptyModuleTest(){
        // redirect system stdout to outstream and save in baos
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream outstream = new PrintStream(baos);
        PrintStream old = System.out; // save the previous System.out
        System.setOut(outstream);

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


        // krun compiledDefinition with empty program pgm
        String pgm = "";
        KRunOptions kRunOptions = new KRunOptions();
        new JCommander(kRunOptions, "--output", "pretty");
        FileUtil files = FileUtil.get(globalOptions, System.getenv());
        RewriterResult krunResult = new Kapi().krun(pgm, null, compiledDef);
        KRun.prettyPrint(compiledDef, kRunOptions.output, s -> KRun.outputFile(s, kRunOptions, files), krunResult.k());

        // test, the krun should output nothing but a new line with exit code 0
        String outstr = baos.toString();
        assertEquals(outstr, "\n");

        // redirect output to stdout again
        System.setOut(old);
        System.out.println(outstr);
    }
 
}