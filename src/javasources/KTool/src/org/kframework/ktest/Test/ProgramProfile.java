// Copyright (c) 2014 K Team. All Rights Reserved.
package org.kframework.ktest.Test;

import java.util.List;

import org.kframework.ktest.PgmArg;

/**
 * A set of information used to specify the behavior for executing some subset of KRun programs.
 * For example, a profile might represent all programs, or it might represent programs individually.
 *
 */
public class ProgramProfile {
    /**
     * The list of command line options to pass to the program.
     */
    private List<PgmArg> args;
    
    /**
     * {@code true} if the program's expected results should be treated as a list of regular expressions, one
     * per line. {@false} to use legacy comparator.
     */
    private boolean regex;
    
    /**
     * Alternative krun executable. (It should reside in KHOME/bin directory.)
     */
    private String krunExec;

    public ProgramProfile(List<PgmArg> args, boolean regex, String krunExec) {
        this.args = args;
        this.regex = regex;
        this.krunExec = krunExec;
    }
    
    public List<PgmArg> getArgs() {
        return args;
    }
    
    public boolean isRegex() {
        return regex;
    }

    public String getKrunExec() {
        return krunExec;
    }
}
