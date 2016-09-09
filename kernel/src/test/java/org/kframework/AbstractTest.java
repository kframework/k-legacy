// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework;

import org.junit.After;
import org.junit.Before;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.errorsystem.KExceptionManager;

public class AbstractTest {
    protected KExceptionManager kem;

    @Before
    public void makeKEM() {
        kem = new KExceptionManager(new GlobalOptions());
    }

    @After
    public void printKEM() { kem.print(); }

}
