package org.kframework.krun;

import com.beust.jcommander.JCommander;
import org.junit.Before;
import org.junit.Test;
import org.kframework.AbstractTest;
import org.kframework.Kapi;
import org.kframework.RewriterResult;
import org.kframework.backend.java.symbolic.JavaBackend;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kompile.Kompile;
import org.kframework.kompile.KompileOptions;
import org.kframework.kore.K;
import org.kframework.main.GlobalOptions;
import org.kframework.main.Tool;
import org.kframework.unparser.OutputModes;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.inject.CommonModule;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URISyntaxException;

import static org.junit.Assert.assertEquals;
 
public class TstKRunOnKORE_IT extends AbstractTest {
    private KompileOptions kompileOptions;
    private GlobalOptions globalOptions;

    // for kompile
    private Kompile kompile;
    private File defintionFile;
    private File definitionDir;
    private String mainModuleName;
    private String mainProgramsModuleName;
    private CompiledDefinition compiledDef;

    // for krun
    Kapi kapi = new Kapi();
    private String pgm;
    KRunOptions kRunOptions;
    FileUtil files;


    @Before
    public void setup() throws URISyntaxException, IOException {
        kompileOptions = new KompileOptions();
        globalOptions = new GlobalOptions();
        kRunOptions = new KRunOptions();
        files = FileUtil.get(globalOptions, System.getenv());
        new JCommander(kRunOptions, "--output", "pretty");
    }

    private String getPrettyPrintOutput(K result, CompiledDefinition compiledDef){
        // redirect system stdout to outstream and save in baos
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream outstream = new PrintStream(baos);
        PrintStream old = System.out; // save the previous System.out
        System.setOut(outstream);

        KRun.prettyPrint(compiledDef, kRunOptions.output, s -> KRun.outputFile(s, kRunOptions, files), result);

        // redirect output to stdout again
        String outstr = baos.toString();
        System.setOut(old);
        System.out.println(outstr);

        return outstr;
    }

    private void generateCompiledDefintion(){
        FileUtil files = new FileUtil(CommonModule.tempDir(definitionDir, Tool.KOMPILE),
                definitionDir, definitionDir, definitionDir, kompileOptions.global, System.getenv());
        kompile = new Kompile(kompileOptions, files, kem, false);
        compiledDef = kompile.run(defintionFile, mainModuleName, mainProgramsModuleName,
                new JavaBackend(kem, files, globalOptions, kompileOptions).steps());
    }

    /* *
     * krunEmptyModuleTest simulates the command for bug#2219
     * `-krun --no-prelude --debug k-distribution/src/test/resources/compiler-tests/empty-module.k --output pretty`
     * The empty-module.k is created with no imports, syntax, rules, and configuration in it.
     * Before bug2219 is fixed, an NoSuchElementException is thrown.
     * After the bug is fixed, an empty output is expected to be generated with an exit code 0.
     */
    @Test
    public void krunNoPreludeEmptyModuleTest(){
        // set --no-prelude and --debug options (default to be false)
        kompileOptions.outerParsing.noPrelude = true;
        globalOptions.debug = true;

        // set definitionFile with its' module names
        definitionDir = new File("src/test/resources/compiler-tests/");
        defintionFile = new File(definitionDir.toString() + "/empty-module.k");
        mainModuleName = "EMPTY-MODULE";
        mainProgramsModuleName = "EMPTY-MODULE";

        generateCompiledDefintion();

        // krun compiledDefinition with empty program pgm
        pgm = "";
        RewriterResult krunResult = new Kapi().krun(pgm, null, compiledDef);
        // test, the krun should output nothing but a new line with exit code 0
        String stdoutString = getPrettyPrintOutput(krunResult.k(), compiledDef);
        assertEquals(stdoutString, "\n");


        // kast compiledDefinition with empty program pgm
        // kRunOptions.output = OutputModes.KAST;
        K kastResult = kapi.kast(pgm, compiledDef);
        String kastOutString = getPrettyPrintOutput(kastResult, compiledDef);
        assertEquals(kastOutString, "\n");

        // pretty print in KAST mode, the output contains Map, STDIN, String, Basic-K
        // may be due to the way to handle definition transformation in kore's transformers.scala,
        // SelectiveDefinitionTransformer function.
        kRunOptions.output = OutputModes.KAST;
        String KASTModeString = getPrettyPrintOutput(kastResult, compiledDef);
        assertEquals(KASTModeString,
           "(`_Map_`(`_Map_`(`.Map`(.KList),`_|->_`"+
           "(#token(\"$STDIN\",\"KConfigVar@BASIC-K\"),"+
           "#token(\"\\\"\\\"\",\"String@STRING-SYNTAX\"))),"+
           "`_|->_`(#token(\"$IO\",\"KConfigVar@BASIC-K\"),"+
           "#token(\"\\\"on\\\"\",\"String@STRING-SYNTAX\"))))\n");

        // pretty print in BINARY mode
        kRunOptions.output = OutputModes.BINARY;
        String BINARYModeString = getPrettyPrintOutput(kastResult, compiledDef);
        assertEquals(BINARYModeString,"\u007FKAST\u0004\u0000\u0000\u0002\u0000\u0000\u0000\u0000\u0000\u0000\u0000" +
                "\u0004\u0000.\u0000M\u0000a\u0000p\u0000\u0000\u0000\u0000\u0000\u0001\u0000\u0000\u0000\u0000\u0000" +
                "\u0000\u0000\u0006\u0000$\u0000S\u0000T\u0000D\u0000I\u0000N\u0000\u0000\u0000\u0000\u0000\u0000\u0000" +
                "\u0012\u0000K\u0000C\u0000o\u0000n\u0000f\u0000i\u0000g\u0000V\u0000a\u0000r\u0000@\u0000B\u0000A" +
                "\u0000S\u0000I\u0000C\u0000-\u0000K\u0001\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0002\u0000" +
                "\"\u0000\"\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0014\u0000S\u0000t\u0000r\u0000i\u0000n" +
                "\u0000g\u0000@\u0000S\u0000T\u0000R\u0000I\u0000N\u0000G\u0000-\u0000S\u0000Y\u0000N\u0000T\u0000A" +
                "\u0000X\u0002\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0005\u0000_\u0000|\u0000-\u0000>\u0000_" +
                "\u0000\u0000\u0000\u0000\u0002\u0002\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0005\u0000_\u0000M" +
                "\u0000a\u0000p\u0000_\u0000\u0000\u0000\u0000\u0002\u0001\u0000\u0000\u0000\u0000\u0000\u0000\u0000" +
                "\u0003\u0000$\u0000I\u0000O\u0000\u0000\u0000\u0006\u0001\u0000\u0000\u0000\u0000\u0000\u0000\u0000" +
                "\u0004\u0000\"\u0000o\u0000n\u0000\"\u0000\u0000\u0000\u0005\u0002\u0000\u0000\u0000\u0004\u0000\u0000" +
                "\u0000\u0000\u0002\u0002\u0000\u0000\u0000\u0003\u0000\u0000\u0000\u0000\u0002\u0002\u0000\u0000\u0000" +
                "\u0000\u0001\u0007\n");
    }

    @Test
    public void krunHasPreludeNonEmptyModuleTest(){
        // set --debug options (default to be false)
        globalOptions.debug = true;

        // set definitionFile with its' module names
        definitionDir = new File("tutorial/1_k/2_imp/lesson_4");
        defintionFile = new File(definitionDir.toString() + "/imp.k");
        mainModuleName = "IMP";
        mainProgramsModuleName = "IMP-SYNTAX";

        generateCompiledDefintion();

        // krun compiledDefinition
        pgm = "int n, sum;\n" +
                "n = 100;\n" +
                "sum = 0;\n" +
                "while (!(n <= 0)) {\n" +
                "  sum = sum + n;\n" +
                "  n = n + -1;\n" +
                "}";
        RewriterResult krunResult = kapi.krun(pgm, null, compiledDef);
        String krunOutString = getPrettyPrintOutput(krunResult.k(), compiledDef);
        assertEquals(krunOutString, "<T> <k> . </k> <state> n |-> 0 sum |-> 5050 </state> </T>\n");

        // kast compiledDefinition
        K kastResult = kapi.kast(pgm, compiledDef);
        String kastOutString = getPrettyPrintOutput(kastResult, compiledDef);
        assertEquals(kastOutString, "initTCell ( .Map $STDIN |-> \"\" $IO |-> \"on\" $PGM |-> int n , ( sum , .Ids ) ;"
                            +" ( n = 100 ; sum = 0 ; while ( ! ( n <= 0 ) ) { sum = sum + n ; n = n + -1 ; } ) )\n");

        // pretty print in KAST mode
        kRunOptions.output = OutputModes.KAST;
        String KASTModeString = getPrettyPrintOutput(kastResult, compiledDef);
        assertEquals(KASTModeString, "initTCell(`_Map_`(`_Map_`(`_Map_`(`.Map`(.KList),`_|->_`"+
                "(#token(\"$STDIN\",\"KConfigVar@BASIC-K\"),#token(\"\\\"\\\"\",\"String@STRING-SYNTAX\"))),"+"" +
                "`_|->_`(#token(\"$IO\",\"KConfigVar@BASIC-K\"),#token(\"\\\"on\\\"\",\"String@STRING-SYNTAX\"))),"+"" +
                "`_|->_`(#token(\"$PGM\",\"KConfigVar@BASIC-K\"),`int_;_`(`_,_`(#token(\"n\",\"Id@ID\"),"+
                "`_,_`(#token(\"sum\",\"Id@ID\"),`.List{\"_,_\"}`(.KList))),"+
                "`__`(`__`(`_=_;`(#token(\"n\",\"Id@ID\"),#token(\"100\",\"Int@INT-SYNTAX\")),"+
                "`_=_;`(#token(\"sum\",\"Id@ID\"),#token(\"0\",\"Int@INT-SYNTAX\"))),"+
                "`while(_)_`(`!_`(`_<=_`(#token(\"n\",\"Id@ID\"),#token(\"0\",\"Int@INT-SYNTAX\"))),"+
                "`{_}`(`__`(`_=_;`(#token(\"sum\",\"Id@ID\"),`_+_`(#token(\"sum\",\"Id@ID\"),"+
                "#token(\"n\",\"Id@ID\"))),`_=_;`(#token(\"n\",\"Id@ID\"),`_+_`(#token(\"n\",\"Id@ID\"),"+
                "#token(\"-1\",\"Int@INT-SYNTAX\")))))))))))\n");

        // pretty print in BINARY mode
        kRunOptions.output = OutputModes.BINARY;
        String BINARYModeString = getPrettyPrintOutput(kastResult, compiledDef);
}