package org.kframework.debugger.controller;

import org.kframework.debugger.backend.Debugger;
import org.kframework.debugger.frontend.*;
import com.google.inject.Inject;
import org.kframework.kil.Attributes;
import org.kframework.kil.Term;
import org.kframework.kil.loader.Context;
import org.kframework.kompile.KompileOptions;
import org.kframework.krun.KRunExecutionException;
import org.kframework.krun.api.*;
import org.kframework.krun.ioserver.commands.Command;
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
        private final Debugger backend;
        private final DebuggerFrontend frontend;
        private final Context context;
        private final FileUtil files;

        @Inject
        Controller(
                KExceptionManager kem,
                @Main Term initialConfiguration,
                @Main KompileOptions kompileOptions,
                BinaryLoader loader,
                @Main Debugger backend,
                @Main DebuggerFrontend frontend,
                @Main Context context,
                @Main FileUtil files) {
            this.kem = kem;
            this.initialConfiguration = initialConfiguration;
            this.kompileOptions = kompileOptions;
            this.loader = loader;
            this.backend = backend;
            this.context = context;
            this.files = files;
            this.frontend = frontend;
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
                @Main Debugger backend,
                @Main DebuggerFrontend frontend,
                @Main Context context,
                @Main FileUtil files) {
            this(kem, initialConfiguration, kompileOptions, loader, backend, frontend, context, files);
            this.statePrinter = statePrinter;
            this.searchPrinter = searchPrinter;
            this.graphPrinter = graphPrinter;
            this.transitionPrinter = transitionPrinter;
        }

        @Override
        public Void run(Void v, Attributes a) {
            //entry point into the entire debugger architecture
            a.add(Context.class, context);
            try{
                backend.start(initialConfiguration);
            }
            catch(UnsupportedBackendOptionException e){
                kem.registerCriticalError("Backend \""
                        + kompileOptions.backend
                        + "\" does not support option " + e.getMessage(), e);
                throw new AssertionError("unreachable");
            }
            catch(KRunExecutionException e){
                kem.registerCriticalError(e.getMessage(), e);
            }

            //initialize the frontend
            FrontendStatus status = frontend.initFrontend();
            if(status == FrontendStatus.FAILURE){
                //throw customized exception
                return null;
            }
            //start the actual debugging loop
            try {
                backend.start(initialConfiguration);
                //wait for the command
                do {
                    DebuggerCommand command = frontend.getCommand();
                    KRunState currentState = backend.getState(backend.getCurrentState());
                    switch(command.getGenericCommand()){
                        case STEP:
                            if(command.getStepCount() > 0){
                                backend.step(command.getStepCount());
                                ControllerResult result = new ControllerResult(
                                        a,
                                        currentState,
                                        backend.getState(backend.getCurrentState()),
                                        command,
                                        null,
                                        statePrinter,
                                        ControllerResult.Status.VALID
                                );
                                frontend.publishResult(result);
                                break;
                            }
                            else{
                               break;
                            }
                        case SET_STATE:
                            backend.setCurrentState(command.getStateOne());
                            break;
                        case GET_STATE:
                            KRunState state = backend.getState(command.getStateOne());
                            ControllerResult result = new ControllerResult(
                                    a,
                                    currentState,
                                    state,
                                    command,
                                    null,
                                    statePrinter,
                                    ControllerResult.Status.VALID
                            );

                            ControllerResult result = new ControllerResult(a);
                            result.setCommand(command);
                            result.setNewState(state);
                            result.setTransformation(statePrinter);
                            frontend.publishResult(result);
                            break;
                        case SEARCH:

                    }

                }while(true);
            }
            catch(Exception e){

            }
            return null;
        }

        @Override
        public String getName() {
            return "--debugger";
        }
    }
}
