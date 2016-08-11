// Copyright (c) 2014-2016 K Team. All Rights Reserved.
package org.kframework.utils;

import com.google.inject.AbstractModule;
import com.google.inject.Injector;
import com.google.inject.Key;
import com.google.inject.Provides;
import com.google.inject.name.Names;
import org.junit.Before;
import org.junit.runner.RunWith;
import org.kframework.kdoc.KDocOptions;
import org.kframework.kil.Configuration;
import org.kframework.kil.Definition;
import org.kframework.kil.loader.Context;
import org.kframework.kompile.KompileOptions;
import org.kframework.krun.KRunOptions;
import org.kframework.krun.RunProcess;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import java.io.File;

@RunWith(MockitoJUnitRunner.class)
public abstract class BaseTestCase {

    @Mock
    protected Context context;

    @Mock
    protected Definition definition;

    @Mock
    protected Configuration configuration;

    @Mock
    protected KExceptionManager kem;

    @Mock
    protected Stopwatch sw;

    @Mock
    protected BinaryLoader loader;

    @Mock
    protected RunProcess rp;

    @Mock
    protected
    File kompiledDir;

    @Mock
    File definitionDir;

    @Mock
    File tempDir;

    @Mock
    protected FileUtil files;

    @Before
    public void setUpWiring() {
        context.kompileOptions = new KompileOptions();
    }
}
