// Copyright (c) 2014-2016 K Team. All Rights Reserved.
package org.kframework.utils.inject;

import org.kframework.main.Tool;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.file.TTYInfo;

import java.io.File;
import java.util.Map;

import static org.fusesource.jansi.internal.CLibrary.*;

public class CommonModule {

    public static File tempDir(File workingDir, Tool tool) {
        return new File(workingDir, FileUtil.generateUniqueFolderName("." + tool.name().toLowerCase()));
    }

    public static ProcessBuilder pb(File workingDir, Map<String, String> env) {
        return new FileUtil(null, null, workingDir, null, null, env).getProcessBuilder();
    }

    public static TTYInfo ttyInfo(Map<String, String> env) {
        boolean stdin, stdout, stderr;
        if (env.containsKey("NAILGUN_TTY_0")) {
            stdin = !env.get("NAILGUN_TTY_0").equals("0");
        } else {
            stdin = isatty(0) != 0;
        }
        if (env.containsKey("NAILGUN_TTY_1")) {
            stdout = !env.get("NAILGUN_TTY_1").equals("0");
        } else {
            stdout = isatty(1) != 0;
        }
        if (env.containsKey("NAILGUN_TTY_2")) {
            stderr = !env.get("NAILGUN_TTY_2").equals("0");
        } else {
            stderr = isatty(2) != 0;
        }
        return new TTYInfo(stdin, stdout, stderr);
    }

}
