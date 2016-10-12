// Copyright (c) 2016 K Team. All Rights Reserved.
package org.kframework.keq;

import com.beust.jcommander.Parameter;
import com.beust.jcommander.ParametersDelegate;
import org.kframework.main.GlobalOptions;
import org.kframework.utils.options.DefinitionLoadingOptions;
import org.kframework.utils.options.SMTOptions;

import java.util.List;

/**
 * Created by daejunpark on 10/12/16.
 */
public class KeqOptions {

    /**
     * Equivalence specification, *_spec.k file.
     */
    @Parameter(description="<file>")
    public List<String> parameters;

    @ParametersDelegate
    public transient GlobalOptions global = new GlobalOptions();

    @ParametersDelegate
    public SMTOptions smt = new SMTOptions();

    // TODO(Daejun): drop this
    @ParametersDelegate
    public DefinitionLoadingOptions definitionLoading = new DefinitionLoadingOptions();

    @Parameter(names="--def0", description="K definition of aggregated configuration.")
    public String def0;

    @Parameter(names="--def1", description="K definition of 1st language.")
    public String def1;

    @Parameter(names="--def2", description="K definition of 2nd language.")
    public String def2;

    @Parameter(names="--mod0", description="Main module name of def0")
    public String mod0;

    @Parameter(names="--mod1", description="Main module name of def1")
    public String mod1;

    @Parameter(names="--mod2", description="Main module name of def2")
    public String mod2;

}
