// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.parser;

import org.kframework.attributes.Source;
import org.kframework.kore.Sort;

import java.io.Serializable;

/**
 * Each module-derived parser needs to implement this interface.
 */
public interface UserParser extends Serializable {
    /**
     * @param toParse the text to be parsed
     * @param fromSource the Source of the String toParse
     * @param startSymbol the sort of the program start symbol (non-terminal)
     * @return an object of ParseResult which includes the K ast and the warnings during the parsing.
     */
    public ParseResult parse(String toParse, Source fromSource, Sort startSymbol);
}
