package org.kframework.krun;
import com.beust.jcommander.JCommander;
import org.junit.Before;
import org.junit.Test;
import org.kframework.AbstractTest;
import org.kframework.Kapi;
import org.kframework.RewriterResult;
import org.kframework.attributes.Source;
import org.kframework.kore.K;
import org.kframework.kore.compile.KtoKORE;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.KoreUtils;
import org.kframework.utils.file.FileUtil;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URISyntaxException;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class TaskCellBag_KrunTest extends AbstractTest {
    private KoreUtils utils;
    private KtoKORE trans;
    private String fileName;
    private KRunOptions kRunOptions;
    private String pgm;
    //private KRun mykrun;

    @Before
    public void setup() throws URISyntaxException, IOException {
        kRunOptions = new KRunOptions();
        fileName = "/convertor-tests/simple-typed-static.k";
        utils = new KoreUtils(fileName, "SIMPLE-TYPED-STATIC", "SIMPLE-TYPED-STATIC-SYNTAX", kem);
        trans = new KtoKORE();
        //mykrun=new KRun();
        pgm= "int factorial(int y) {\n" +
                "  print(\"Factorial of \", y, \" is: \");\n" +
                "  int t=1;\n" +
                "  for(int i=1; i<=y; ++i) {\n" +
                "    t = t*i;\n" +
                "  }\n" +
                "  return t;\n" +
                "}\n" +
                "\n" +
                "void main() {\n" +
                "  print(\"Input a natural number: \");\n" +
                "  print(factorial(read()),\"\\n\");\n" +"}";
    }
    @Test
    public void TestT(){
        // redirect system stdout to outstream and save in baos
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream outstream = new PrintStream(baos);
        PrintStream old = System.out; // save the previous System.out
        System.setOut(outstream);

        // run command and pretty print in output format mode
        KRunOptions krunOptions = new KRunOptions();
        new JCommander(kRunOptions, "--pattern", "\"<tasks> .TaskCellBag </tasks>\"");
        GlobalOptions globalOptions = new GlobalOptions();
        FileUtil files = FileUtil.get(globalOptions, System.getenv());
        Kapi kapi = new Kapi();
        RewriterResult result = kapi.krun(pgm, null, utils.compiledDef);
        KRun.prettyPrint(utils.compiledDef, krunOptions.output, s -> KRun.outputFile(s, krunOptions, files), result.k());
        //mykrun.printK(result.k(),krunOptions,utils.compiledDef);
        // check result
        String outstr = baos.toString();
        System.out.println(outstr);
        final String exp = "No search Results";
        //assertEquals(exp , outstr);
    }
}


