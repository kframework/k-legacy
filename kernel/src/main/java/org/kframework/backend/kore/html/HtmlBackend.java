package org.kframework.backend.kore.html;

import com.google.inject.Inject;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kore.kdoc.PosterBackend;

/**
 * Created by eddie on 9/14/15.
 */
public class HtmlBackend extends PosterBackend {
    @Inject
    HtmlBackend() {}

    @Override
    public void run(CompiledDefinition definition) {
        // TODO
        System.out.println("HTML backend.");
    }

}

