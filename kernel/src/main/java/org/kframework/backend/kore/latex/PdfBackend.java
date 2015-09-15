package org.kframework.backend.kore.latex;

import com.google.inject.Inject;
import org.kframework.definition.Definition;
import org.kframework.kore.kdoc.PosterBackend;

/**
 * Created by eddie on 9/14/15.
 */
public class PdfBackend extends PosterBackend {
    @Inject
    PdfBackend() {}

    @Override
    public void run(Definition definition) {
        // TODO
    }
}
