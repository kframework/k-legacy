package org.kframework;

import org.junit.After;
import org.junit.Before;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.errorsystem.KExceptionManager;

/**
 * Created by cos on 12/12/15.
 */
public class AbstractTest {
    protected KExceptionManager kem;

    @Before
    public void makeKEM() {
        kem = new KExceptionManager(new GlobalOptions());
    }

    @After
    public void printKEM() { kem.print(); }

}
