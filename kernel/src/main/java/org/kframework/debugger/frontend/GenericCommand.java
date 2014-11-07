// Copyright (c) 2013-2014 K Team. All Rights Reserved.

package org.kframework.debugger.frontend;

/**
 * Enumeration representing commands that the frontend can
 * give to the Controller.
 */
public enum GenericCommand {
    STEP, SEARCH, SELECT, SET_STATE, GET_STATE,
    EXIT, GET_GRAPH, GET_TRANSITION, SAVE, LOAD, INVALID,
    RESUME
}
