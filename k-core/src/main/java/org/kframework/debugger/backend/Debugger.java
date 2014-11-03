package org.kframework.debugger.backend;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import jline.ArgumentCompletor;
import jline.Completor;
import jline.ConsoleReader;
import jline.FileNameCompletor;
import jline.MultiCompletor;
import jline.SimpleCompletor;

import org.kframework.backend.unparser.PrintTransition;
import org.kframework.kil.Attributes;
import org.kframework.kil.StringBuiltin;
import org.kframework.kil.Term;
import org.kframework.kil.loader.Context;
import org.kframework.kompile.KompileOptions;
import org.kframework.krun.KRunDebuggerOptions;
import org.kframework.krun.KRunExecutionException;
import org.kframework.krun.api.KRunGraph;
import org.kframework.krun.api.KRunState;
import org.kframework.krun.api.SearchResults;
import org.kframework.krun.api.Transition;
import org.kframework.krun.api.UnsupportedBackendOptionException;
import org.kframework.transformation.Transformation;
import org.kframework.utils.BinaryLoader;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.inject.InjectGeneric;
import org.kframework.utils.inject.Main;

import com.beust.jcommander.JCommander;
import com.beust.jcommander.ParameterException;
import com.google.inject.Inject;
import com.sun.org.apache.xerces.internal.impl.dv.util.Base64;

public interface Debugger {

    /**
     * Get the current graph of the explored state space in the transition system.
     *
     * @return A graph whose nodes and edges describe the explored state of the program.
     */
    public abstract KRunGraph getGraph();

    /**
     * Reset the debugger's configuration to the graph specified by the parameter
     */
    public abstract void setGraph(KRunGraph graph);

    /**
     * Get the state number of the currently selected state.
     *
     * @return An unique integer identifying the currently selected state; null if no state is selected
     */
    public abstract Integer getCurrentState();

    /**
     * Set the selected state of the debugger.
     *
     * @param stateNum An integer uniquely identifying the state to select; null to deselect current
     *                 state.
     * @throws IllegalArgumentException Thrown if the specified state is not a valid state in the
     *                                  graph (or null)
     */
    public abstract void setCurrentState(Integer stateNum);

    /**
     * Get a state by its state number.
     *
     * @param stateNum An integer uniquely identifying the state to select
     * @return The node in the explored state graph with the specified state number.
     * @throws IllegalArgumentException Thrown if the state number is not in the graph
     */
    public abstract KRunState getState(int stateNum);

    /**
     * Get the edge connecting two states.
     *
     * @param state1 The integral state number of the origin state of the edge.
     * @param state2 The integral state number of the destination state of the edge.
     * @return Information concerning the transition in the transition system which connected state1 to
     * state2
     * @throws IllegalArgumentException Thrown if no edge connects state1 and state2
     */
    public abstract Transition getEdge(int state1, int state2);

    /**
     * Step a particular number of transitions. This function returns when the specified number of
     * steps have been taken, or when rewriting has terminated.
     *
     * @param steps The maximum number of transitions to follow through (0 to stop before first
     *              transition)
     */
    public abstract void step(int steps) throws KRunExecutionException;

    /**
     * Explore the complete search graph from the currently selected state forward a specified number
     * of steps.
     *
     * @param steps The maximum depth of the search to perform.
     * @return A set of all new states discovered by the stepper after searching the specified number
     * of steps.
     */
    public abstract SearchResults stepAll(int steps) throws KRunExecutionException;

    /**
     * Explore the search graph one step at a time until rewriting terminates.
     */
    public abstract void resume() throws KRunExecutionException;

    /**
     * Read a string and append it to the buffer for stdin.
     *
     * @param s The string to append to stdin.
     */
    public abstract void readFromStdin(String s);

    /**
     * Start debugging from a particular term
     */
    public abstract void start(Term initialConfiguration) throws KRunExecutionException;
}