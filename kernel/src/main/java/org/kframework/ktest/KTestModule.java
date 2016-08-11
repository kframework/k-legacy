// Copyright (c) 2014-2016 K Team. All Rights Reserved.
package org.kframework.ktest;

import java.io.File;

import org.kframework.krun.ColorOptions;
import org.kframework.ktest.CmdArgs.KTestOptions;
import org.kframework.main.FrontEnd;
import org.kframework.main.GlobalOptions;
import org.kframework.main.Tool;
import com.google.inject.AbstractModule;
import com.google.inject.Provides;
import com.google.inject.TypeLiteral;
import com.google.inject.multibindings.Multibinder;
import com.google.inject.util.Providers;

public class KTestModule {

    @Provides
    GlobalOptions globalOptions(KTestOptions options) {
        return options.getGlobal();
    }

    @Provides
    ColorOptions colorOptions(KTestOptions options) {
        return options.getColorOptions();
    }

}
