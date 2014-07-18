// Copyright (c) 2014 K Team. All Rights Reserved.
package org.kframework.backend.java.symbolic;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.kframework.backend.java.builtins.BoolToken;
import org.kframework.backend.java.kil.BuiltinMap;
import org.kframework.backend.java.kil.Definition;
import org.kframework.backend.java.kil.TermContext;
import org.kframework.kil.loader.Context;
import org.kframework.utils.options.SMTOptions;
import org.kframework.utils.options.SMTSolver;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class UseSMTTest {

    @Mock
    TermContext tc;

    @Mock
    Context context;

    @Mock
    Definition definition;

    @Before
    public void setUp() {
        when(tc.definition()).thenReturn(definition);
        when(tc.definition().context()).thenReturn(context);
        context.smtOptions = new SMTOptions();
        context.smtOptions.smt = SMTSolver.Z3;
    }

    @Test
    public void testGetModel() {
        System.err.println(System.getProperty("java.library.path"));
        BuiltinMap.Builder builder = new BuiltinMap.Builder();
        assertEquals(builder.build(), UseSMT.checkSat(BoolToken.TRUE, tc));
    }
}
