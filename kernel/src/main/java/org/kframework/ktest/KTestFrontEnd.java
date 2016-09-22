// Copyright (c) 2013-2016 K Team. All Rights Reserved.
package org.kframework.ktest;

import org.apache.commons.io.FilenameUtils;
import org.kframework.ktest.CmdArgs.KTestOptions;
import org.kframework.ktest.Config.ConfigFileParser;
import org.kframework.ktest.Config.InvalidConfigError;
import org.kframework.ktest.Config.LocationData;
import org.kframework.ktest.Test.TestCase;
import org.kframework.ktest.Test.TestSuite;
import org.kframework.main.FrontEnd;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.file.JarInfo;
import org.kframework.utils.inject.JCommanderModule;
import org.kframework.utils.inject.CommonModule;
import org.xml.sax.SAXException;

import com.beust.jcommander.ParameterException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

import java.io.File;
import java.io.IOException;
import java.util.*;


public class KTestFrontEnd extends FrontEnd {

    private final KTestOptions options;
    private final KExceptionManager kem;
    private final Map<String, String> env;
    private final FileUtil files;

    public KTestFrontEnd(
            KTestOptions options,
            KExceptionManager kem,
            GlobalOptions globalOptions,
            Map<String, String> env,
            FileUtil files) {
        super(kem, globalOptions, files);
        this.options = options;
        this.options.setDebug(globalOptions.debug);
        this.options.setWarnings2errors(globalOptions.warnings2errors);
        this.kem = kem;
        this.env = env;
        this.files = files;
    }

    public int run() {
        try {
            options.validateArgs(files);
            return makeTestSuite(options.getTargetFile(), options).run() ? 0 : 1;
        } catch (SAXException | ParserConfigurationException | IOException | TransformerException
                | ParameterException e) {
            throw KEMException.criticalError(e.getMessage(), e);
        } catch (InvalidConfigError e) {
            LocationData location = e.getLocation();
            throw KEMException.criticalError(e.getMessage(), e, location.getLocation(), location.getSource());
        }
    }

    private TestSuite makeTestSuite(String targetFile, KTestOptions cmdArgs) throws SAXException,
            TransformerException, ParserConfigurationException, IOException, InvalidConfigError {
        TestSuite ret;
        switch (FilenameUtils.getExtension(targetFile)) {
        case "xml":
            ret = new TestSuite(new ConfigFileParser(
                    new File(cmdArgs.getTargetFile()), cmdArgs, env, kem, files).parse(), cmdArgs, files);
            break;
        case "k":
            TestCase tc = TestCase.makeTestCaseFromK(cmdArgs, kem, files, env);
            tc.validate();
            List<TestCase> tcs = new LinkedList<>();
            tcs.add(tc);
            ret = new TestSuite(tcs, cmdArgs, files);
            break;
        default:
            // this code should be unreacable, because `validateArgs' should ensure that
            // targetFile extension can only be .k or .xml
            ret = null; assert false;
        }
        return ret;
    }
}
