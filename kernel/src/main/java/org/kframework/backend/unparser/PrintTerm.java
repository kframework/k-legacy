// Copyright (c) 2014-2015 K Team. All Rights Reserved.
package org.kframework.backend.unparser;

import java.io.IOException;
import java.io.InputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;

import org.apache.commons.lang3.tuple.Pair;
import org.kframework.kil.ASTNode;
import org.kframework.kil.Attributes;
import org.kframework.kil.loader.Context;
import org.kframework.krun.ColorOptions;
import org.kframework.transformation.Transformation;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;

import com.google.inject.Inject;

public class PrintTerm implements Transformation<ASTNode, InputStream> {

    private final ColorOptions colorOptions;
    private final OutputModes mode;

    @Inject
    public PrintTerm(
            ColorOptions colorOptions,
            OutputModes mode) {
        this.colorOptions = colorOptions;
        this.mode = mode;
    }

    @Override
    public InputStream run(ASTNode node, Attributes a) {
        Pair<PipedInputStream, PipedOutputStream> pipe = FileUtil.pipeOutputToInput();
        new Thread(() ->
        {
            try (PipedOutputStream out = pipe.getRight()) {
                new Unparser(a.typeSafeGet(Context.class),
                        colorOptions.color(), colorOptions.terminalColor(),
                        mode != OutputModes.NO_WRAP, false).print(out, node);
            } catch (IOException e) {
                throw KExceptionManager.criticalError("Error writing unparsed output to output stream.");
            }
        }).start();
        return pipe.getLeft();
    }

    @Override
    public String getName() {
        return "print term";
    }

}
