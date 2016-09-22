// Copyright (c) 2014-2016 K Team. All Rights Reserved.
package org.kframework.ktest;

import java.io.File;

import org.kframework.krun.ColorOptions;
import org.kframework.ktest.CmdArgs.KTestOptions;
import org.kframework.main.FrontEnd;
import org.kframework.main.GlobalOptions;
import org.kframework.main.Tool;

public class KTestModule {

    // TODO(Daejun): remove this module

    GlobalOptions globalOptions(KTestOptions options) {
        return options.getGlobal();
    }

    ColorOptions colorOptions(KTestOptions options) {
        return options.getColorOptions();
    }

}
