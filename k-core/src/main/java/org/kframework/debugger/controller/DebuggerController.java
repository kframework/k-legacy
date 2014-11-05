package org.kframework.debugger.controller;

import org.kframework.debugger.backend.Debugger;
import com.google.inject.Inject;
import org.kframework.kil.Attributes;
import org.kframework.kil.Term;
import org.kframework.kil.loader.Context;
import org.kframework.kompile.KompileOptions;
import org.kframework.krun.api.KRunGraph;
import org.kframework.krun.api.KRunState;
import org.kframework.krun.api.SearchResults;
import org.kframework.krun.api.Transition;
import org.kframework.transformation.Transformation;
import org.kframework.utils.BinaryLoader;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.inject.InjectGeneric;
import org.kframework.utils.inject.Main;

/**
 * The Controller Class that acts as a sandwich layer between the frontend
 * and the backend.
 */
public class DebuggerController {

    public static class Controller implements Transformation<Void, Void> {
        private final KExceptionManager kem;
        private final Term initialConfiguration;
        private final KompileOptions kompileOptions;
        private final BinaryLoader loader;
        @InjectGeneric private Transformation<KRunState, String> statePrinter;
        @InjectGeneric private Transformation<SearchResults, String> searchPrinter;
        @InjectGeneric private Transformation<KRunGraph, String> graphPrinter;
        @InjectGeneric private Transformation<Transition, String> transitionPrinter;
        private final Debugger debugger;
        private final Context context;
        private final FileUtil files;

        @Inject
        Controller(
                KExceptionManager kem,
                @Main Term initialConfiguration,
                @Main KompileOptions kompileOptions,
                BinaryLoader loader,
                @Main Debugger debugger,
                @Main Context context,
                @Main FileUtil files) {
            this.kem = kem;
            this.initialConfiguration = initialConfiguration;
            this.kompileOptions = kompileOptions;
            this.loader = loader;
            this.debugger = debugger;
            this.context = context;
            this.files = files;
        }

        Controller(
                KExceptionManager kem,
                @Main Term initialConfiguration,
                @Main KompileOptions kompileOptions,
                BinaryLoader loader,
                Transformation<KRunState, String> statePrinter,
                Transformation<SearchResults, String> searchPrinter,
                Transformation<KRunGraph, String> graphPrinter,
                Transformation<Transition, String> transitionPrinter,
                @Main Debugger debugger,
                @Main Context context,
                @Main FileUtil files) {
            this(kem, initialConfiguration, kompileOptions, loader, debugger, context, files);
            this.statePrinter = statePrinter;
            this.searchPrinter = searchPrinter;
            this.graphPrinter = graphPrinter;
            this.transitionPrinter = transitionPrinter;
        }

        @Override
        public Void run(Void aVoid, Attributes attrs) {
            System.out.println("Successful injection");

            return null;
        }

        @Override
        public String getName() {
            return null;
        }
    }
}
