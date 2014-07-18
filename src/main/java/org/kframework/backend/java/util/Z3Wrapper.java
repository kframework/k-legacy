// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.backend.java.util;

import com.google.common.io.Files;
import com.microsoft.z3.Solver;
import com.microsoft.z3.Status;
import com.microsoft.z3.Z3Exception;

import org.kframework.utils.file.JarInfo;

import java.io.File;
import java.io.IOException;

/**
 * @author Traian
 */
public class Z3Wrapper {

    public static boolean initialized = false;

    public static final Z3Wrapper Z3_WRAPPER = new Z3Wrapper();
    public static Z3Wrapper instance() {
        return Z3_WRAPPER;
    }

    public final String SMT_PRELUDE;

    public Z3Wrapper() {
        String s = "";
        try {
            s = new String(Files.toByteArray(new File(JarInfo.getZ3PreludePath())));
        } catch (IOException e) {
            e.printStackTrace();
        }
        SMT_PRELUDE = s;
    }

    public boolean checkQuery(String query) {
        boolean result = false;
        try {
            com.microsoft.z3.Context context = new com.microsoft.z3.Context();
            Solver solver = context.MkSolver();
            solver.Assert(context.ParseSMTLIB2String(SMT_PRELUDE + query, null, null, null, null));
            result = solver.Check() == Status.UNSATISFIABLE;
            context.Dispose();
        } catch (Z3Exception e) {
            e.printStackTrace();
        }
        return result;
    }
}

