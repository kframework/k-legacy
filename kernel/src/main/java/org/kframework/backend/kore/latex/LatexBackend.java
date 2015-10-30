package org.kframework.backend.kore.latex;

import com.google.inject.Inject;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kore.kdoc.PosterBackend;

/**
 * Created by eddie on 9/14/15.
 */
public class LatexBackend extends PosterBackend {
    @Inject
    LatexBackend() {}

    @Override
    public void run(CompiledDefinition definition) {
        // TODO
        System.out.println("Latex backend");
    }
}
