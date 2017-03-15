// Copyright (c) 2015-2016 K Team. All Rights Reserved.

package org.kframework.kore.compile;

import org.junit.Before;
import org.junit.Test;
import org.kframework.definition.ModuleTransformerException;
import org.kframework.utils.errorsystem.KEMException;
import org.junit.rules.TestName;
import org.kframework.attributes.Source;
import org.kframework.backend.Backends;
import org.kframework.builtin.Sorts;
import org.kframework.definition.Module;
import org.kframework.kale.KaleBackend;
import org.kframework.kale.KaleRewriter;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kompile.Kompile;
import org.kframework.kompile.KompileOptions;
import org.kframework.kore.K;
import org.kframework.kore.KApply;
import org.kframework.kore.KORE;
import org.kframework.kore.KToken;
import org.kframework.main.GlobalOptions;
import org.kframework.parser.ProductionReference;
import org.kframework.unparser.AddBrackets;
import org.kframework.unparser.KOREToTreeNodes;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;

import static org.junit.Assert.*;
import static org.kframework.kore.KORE.*;

public class improperRuleErrorTest {
    private KompileOptions kompileOptions;
    private File definitionFile;
    private KExceptionManager kem;
    private String mainModuleName;
    private String fileName;
    @org.junit.Rule
    public TestName name = new TestName();

    protected File testResource(String baseName) throws URISyntaxException {
        return new File(baseName);
    }

    @Before
    public void main() throws IOException, URISyntaxException {
        fileName = "tutorial\\2_languages\\1_simple\\2_typed\\1_static\\simple-typed-static.k";
        mainModuleName = "SIMPLE-TYPED-STATIC";
        kem = new KExceptionManager(new GlobalOptions());
        definitionFile = testResource(fileName);
        kompileOptions = new KompileOptions();
        kompileOptions.backend = Backends.KALE;
    }

    @Test
    public void testAfterEnhancement() throws IOException, URISyntaxException {
        String error = "";
        Kompile kompile = new Kompile(kompileOptions, FileUtil.testFileUtil(), kem, false);
        try {
            CompiledDefinition compiledDef = kompile.run(definitionFile, mainModuleName, mainModuleName,  new KaleBackend(kompileOptions, kem).steps());
        }
        catch(Throwable e) {
            error = e.toString();
        }
        assertTrue(error.contains("Incomplete rule assignment") );
    }

    /*
    @Test
    public void testBeforeEnhancement() throws IOException, URISyntaxException {
        String error = "";
        Kompile kompile = new Kompile(kompileOptions, FileUtil.testFileUtil(), kem, false);
        try {
            CompiledDefinition compiledDef = kompile.run(definitionFile, mainModuleName, mainModuleName,  new KaleBackend(kompileOptions, kem).steps());
        }
        catch(Throwable e) {
            error = e.toString();
        }
        assertFalse(error.contains("Incomplete rule assignment") );
    }
    */
}