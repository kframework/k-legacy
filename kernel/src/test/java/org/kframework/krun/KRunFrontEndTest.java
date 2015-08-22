// Copyright (c) 2014-2015 K Team. All Rights Reserved.
package org.kframework.krun;

import com.google.inject.util.Providers;
import org.junit.Test;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.BaseTestCase;
import org.kframework.utils.file.JarInfo;
import org.mockito.Mock;

import static org.mockito.Mockito.*;

public class KRunFrontEndTest extends BaseTestCase {

    @Mock
    JarInfo jarInfo;

    @Test
    public void testVersion() {
        GlobalOptions options = new GlobalOptions();
        options.version = true;
        KRunFrontEnd frontend = new KRunFrontEnd(options, null, null, kem, jarInfo, files, scope, Providers.of(kompiledDir), new KRunOptions(), null, null, null, null);
        frontend.main();
        verify(jarInfo).printVersionMessage();
    }
}
