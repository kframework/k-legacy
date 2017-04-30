package org.kframework.kompile;
import com.beust.jcommander.JCommander;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.kframework.AbstractTest;
import org.kframework.Kapi;
import org.kframework.RewriterResult;
import org.kframework.attributes.Source;
import org.kframework.kore.K;
import org.kframework.kore.compile.KtoKORE;
import org.kframework.krun.KRunOptions;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.KoreUtils;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.file.FileUtil;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URISyntaxException;
import static org.junit.Assert.assertEquals;
import java.util.Stack;
import java.util.HashSet;
import static org.junit.Assert.assertTrue;


public class KompileTest extends AbstractTest {
    private KoreUtils utils;
    private K parsed;
    private KtoKORE trans;
    private String fileName;
    private KRunOptions kRunOptions;
    private String pgm;
    private final ByteArrayOutputStream outContent = new ByteArrayOutputStream();


    @Rule
    public ExpectedException thrown= ExpectedException.none();

    /**
     * This test aims to test the fix of bug 2213, once kompile the file Bags.k, exception will be thrown and caught
     * @throws URISyntaxException
     * @throws IOException
     * @throws KEMException
     */
    @Test
    public void KompileTest() throws URISyntaxException, IOException, KEMException {
        // try {
        fileName = "/convertor-tests/Bags.k";


        thrown.expect(KEMException.class);
        thrown.expectMessage("[Error] Internal: java.lang.AssertionError: assertion failed");

        utils = new KoreUtils(fileName, "TEST", "TEST-SYNTAX", kem);


    }


}






