package org.kframework.backend.kore.latex;

import com.google.inject.Inject;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kore.kdoc.PosterBackend;

/**
 * Created by eddie on 9/14/15.
 */
public class PdfBackend extends PosterBackend {
    @Inject
    PdfBackend() {}

    @Override
    public void run(CompiledDefinition definition) {
        System.out.println("In Pdf backend");
        // TODO
    }
}
