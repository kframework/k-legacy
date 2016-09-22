// Copyright (c) 2014-2016 K Team. All Rights Reserved.
package org.kframework.utils.inject;

import java.util.Set;

import org.kframework.main.Tool;
import org.kframework.utils.Stopwatch;
import org.kframework.utils.StringUtil;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.options.SortedParameterDescriptions;

import com.beust.jcommander.JCommander;
import com.beust.jcommander.ParameterException;

public class JCommanderModule  {

    public static JCommander jcommander(String[] args, Tool tool, Set<Object> options, Set<Class<?>> experimentalOptions, KExceptionManager kem, Stopwatch sw) {
        try {
            JCommander jc = new JCommander(options.toArray(new Object[options.size()]), args);
            jc.setProgramName(tool.name().toLowerCase());
            jc.setParameterDescriptionComparator(new SortedParameterDescriptions(experimentalOptions.toArray(new Class<?>[experimentalOptions.size()])));
            sw.printIntermediate("Parse command line options");
            return jc;
        } catch (ParameterException e) {
            throw KEMException.criticalError(e.getMessage(), e);
        }
    }

    public static String usage(JCommander jc) {
        StringBuilder sb = new StringBuilder();
        jc.usage(sb);
        return StringUtil.finesseJCommanderUsage(sb.toString(), jc)[0];
    }

    public static String experimentalUsage(JCommander jc) {
        StringBuilder sb = new StringBuilder();
        jc.usage(sb);
        return StringUtil.finesseJCommanderUsage(sb.toString(), jc)[1];
    }
}
