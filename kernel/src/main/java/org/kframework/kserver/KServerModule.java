// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.kserver;
import java.io.File;

import org.kframework.main.FrontEnd;
import org.kframework.main.GlobalOptions;
import org.kframework.main.Tool;

import com.google.inject.AbstractModule;
import com.google.inject.Provides;
import com.google.inject.TypeLiteral;
import com.google.inject.multibindings.Multibinder;
import com.google.inject.util.Providers;

public class KServerModule {

    @Provides
    GlobalOptions globalOptions(KServerOptions options) {
        return options.global;
    }

}
