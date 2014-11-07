package org.kframework.debugger.frontend;

/**
 * Enumeration representing commands that the frontend can
 * give to the Controller.
 */
public enum GenericCommand {
    STEP, STEP_ALL, SEARCH, SET_STATE, GET_STATE,
    EXIT, GET_GRAPH, GET_TRANSITION, SAVE, LOAD, INVALID
}
