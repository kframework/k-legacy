// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.backend.java.builtins;

import java.io.IOException;
import java.nio.charset.CharacterCodingException;

import org.kframework.backend.java.kil.*;
import org.kframework.kil.loader.Context;
import org.kframework.kil.visitors.exceptions.ParseFailedException;
import org.kframework.krun.K;
import org.kframework.krun.RunProcess;
import org.kframework.krun.api.io.FileSystem;

/**
 * Table of {@code public static} methods for builtin IO operations.
 */
public class BuiltinIOOperations {

    public static FileSystem fs(State state) {
        return state.fileSystem();
    }

    public static Definition definition(State state) {
        return state.definition();
    }

    public static Context state(State state) {
        return state.definition().context();
    }

    public static Term open(StringToken term1, StringToken term2, State state) {
        try {
            return IntToken.of(fs(state).open(term1.stringValue(), term2.stringValue()));
        } catch (IOException e) {
            return processIOException(e.getMessage(), state);
        }
    }

    public static Term tell(IntToken term, State state) {
        try {
            return IntToken.of(fs(state).get(term.longValue()).tell());
        } catch (IOException e) {
            return processIOException(e.getMessage(), state);
        }
    }

    public static Term getc(IntToken term, State state) {
        try {
            return IntToken.of(fs(state).get(term.longValue()).getc() & 0xff);
        } catch (IOException e) {
            return processIOException(e.getMessage(), state);
        }
    }

    public static Term read(IntToken term1, IntToken term2, State state) {
        try {
            return StringToken.of(fs(state).get(term1.longValue()).read(term2.intValue()));
        } catch (IOException e) {
            return processIOException(e.getMessage(), state);
        }
    }

    public static Term close(IntToken term, State state) {
        try {
            fs(state).close(term.longValue());
            return new KItem(new KLabelInjection(new KSequence()), new KList(), state);
        } catch (IOException e) {
            return processIOException(e.getMessage(), state);
        }
    }

    public static Term seek(IntToken term1, IntToken term2, State state) {
        try {
            fs(state).get(term1.longValue()).seek(term2.longValue());
            return new KItem(new KLabelInjection(new KSequence()), new KList(), state);
        } catch (IOException e) {
            return processIOException(e.getMessage(), state);
        }
    }

    public static Term putc(IntToken term1, IntToken term2, State state) {
        try {
            fs(state).get(term1.longValue()).putc(term2.unsignedByteValue());
            return new KItem(new KLabelInjection(new KSequence()), new KList(), state);
        } catch (IOException e) {
            return processIOException(e.getMessage(), state);
        }
    }

    public static Term write(IntToken term1, StringToken term2, State state) {
        try {
            fs(state).get(term1.longValue()).write(term2.byteArrayValue());
            return new KItem(new KLabelInjection(new KSequence()), new KList(), state);
        } catch (CharacterCodingException e) {
            throw new IllegalArgumentException(e);
        } catch (IOException e) {
            return processIOException(e.getMessage(), state);
        }
    }

    public static Term parse(StringToken term1, StringToken term2, State state) {
        try {
            RunProcess rp = new RunProcess();
            org.kframework.kil.Term kast = rp.runParser(K.parser, term1.stringValue(), true, term2.stringValue(), state(state));
            Term term = Term.of(kast, definition(state));
            term = term.evaluate(state);
            return term;
        } catch (ParseFailedException e) {
            return processIOException("noparse", state);
        }
    }

    private static KItem processIOException(String errno, State state) {
        String klabelString = "'#" + errno;
        Definition def = state.definition();
        KLabelConstant klabel = KLabelConstant.of(klabelString, State.of(def));
        assert def.kLabels().contains(klabel) : "No KLabel in definition for errno '" + errno + "'";
        return new KItem(klabel, new KList(), state);
    }
}
