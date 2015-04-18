// Copyright (c) 2014-2015 K Team. All Rights Reserved.
package org.kframework.backend.java.symbolic;

import org.kframework.backend.Backend;
import org.kframework.backend.java.indexing.IndexingTable;
import org.kframework.backend.java.kil.Definition;
import org.kframework.backend.java.kil.GlobalContext;
import org.kframework.krun.api.io.FileSystem;
import org.kframework.krun.ioserver.filesystem.portable.PortableFileSystem;

import com.google.inject.AbstractModule;
import com.google.inject.Provides;
import com.google.inject.multibindings.MapBinder;

public class JavaSymbolicKompileModule extends AbstractModule {

    @Override
    protected void configure() {
        bind(JavaExecutionOptions.class).toInstance(new JavaExecutionOptions());
        bind(Boolean.class).annotatedWith(FreshRules.class).toInstance(true);
        bind(FileSystem.class).to(PortableFileSystem.class);

        MapBinder<String, Backend> mapBinder = MapBinder.newMapBinder(
                binder(), String.class, Backend.class);
        mapBinder.addBinding("java").to(JavaSymbolicBackend.class);
    }

    @Provides
    Definition definition(GlobalContext context) {
        return context.getDefinition();
    }
}
