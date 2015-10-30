// Copyright (c) 2014-2015 K Team. All Rights Reserved.
package org.kframework.kdoc;

import java.util.Map;

import org.kframework.backend.Backends;
import org.kframework.backend.PosterBackend;
import org.kframework.backend.html.HtmlBackend;
import org.kframework.backend.latex.DocumentationBackend;
import org.kframework.backend.latex.LatexBackend;
import org.kframework.backend.latex.PdfBackend;
import org.kframework.backend.unparser.UnflattenBackend;
import org.kframework.backend.unparser.UnparserBackend;
import org.kframework.kil.loader.Context;
import org.kframework.main.FrontEnd;
import org.kframework.main.GlobalOptions;
import org.kframework.main.Tool;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.inject.DefinitionLoadingModule;
import org.kframework.utils.inject.Main;
import org.kframework.utils.inject.Options;
import org.kframework.utils.options.DefinitionLoadingOptions;

import com.google.inject.AbstractModule;
import com.google.inject.Provides;
import com.google.inject.TypeLiteral;
import com.google.inject.multibindings.MapBinder;
import com.google.inject.multibindings.Multibinder;

public class KDocModule extends AbstractModule {

    @Override
    protected void configure() {
        bind(FrontEnd.class).to(KDocFrontEnd.class);
        bind(Tool.class).toInstance(Tool.KDOC);

        install(new DefinitionLoadingModule());

        bind(Context.class).annotatedWith(Main.class).to(Context.class);

        Multibinder<Object> optionsBinder = Multibinder.newSetBinder(binder(), Object.class, Options.class);
        optionsBinder.addBinding().to(KDocOptions.class);
        Multibinder.newSetBinder(binder(), new TypeLiteral<Class<?>>() {
        }, Options.class);

        MapBinder<String, PosterBackend> posterBinder = MapBinder.newMapBinder(
                binder(), String.class, PosterBackend.class);
        posterBinder.addBinding(Backends.PDF).to(PdfBackend.class);
        posterBinder.addBinding(Backends.LATEX).to(LatexBackend.class);
        posterBinder.addBinding(Backends.DOC).to(DocumentationBackend.class);
        posterBinder.addBinding(Backends.HTML).to(HtmlBackend.class);
        posterBinder.addBinding(Backends.UNPARSE).to(UnparserBackend.class);
        posterBinder.addBinding(Backends.UNFLATTEN).to(UnflattenBackend.class);

        MapBinder<String, org.kframework.kore.kdoc.PosterBackend> korePosterBinder = MapBinder.newMapBinder(
                binder(), String.class, org.kframework.kore.kdoc.PosterBackend.class);
        korePosterBinder.addBinding(Backends.HTML).to(org.kframework.backend.kore.html.HtmlBackend.class);
        korePosterBinder.addBinding(Backends.LATEX).to(org.kframework.backend.kore.latex.LatexBackend.class);
        korePosterBinder.addBinding(Backends.PDF).to(org.kframework.backend.kore.latex.PdfBackend.class);
    }

    @Provides
    GlobalOptions globalOptions(KDocOptions options) {
        return options.global;
    }

    @Provides
    DefinitionLoadingOptions defLoadingOptions(KDocOptions options) {
        return options.definitionLoading;
    }

    @Provides
    PosterBackend getBackend(KDocOptions options, Map<String, PosterBackend> map, KExceptionManager kem) {
        PosterBackend backend = map.get(options.format);
        if (backend == null) {
            throw KEMException.criticalError("Invalid poster format: " + options.format
                    + ". It should be one of " + map.keySet());
        }
        return backend;
    }

    @Provides
    org.kframework.kore.kdoc.PosterBackend getKoreBackend(KDocOptions options, Map<String, org.kframework.kore.kdoc.PosterBackend> map, KExceptionManager kem) {
        org.kframework.kore.kdoc.PosterBackend backend = map.get(options.format);
        if (backend == null) {
            throw KEMException.criticalError("Invalid poster format: " + options.format
                    + ". It should be one of " + map.keySet());
        }
        return backend;
    }

}
