// Copyright (c) 2012-2016 K Team. All Rights Reserved.
package org.kframework.backend.latex;

import org.apache.commons.io.FilenameUtils;
import org.kframework.backend.PosterBackend;
import org.kframework.kil.Definition;
import org.kframework.kil.loader.Context;
import org.kframework.kompile.KompileOptions;
import org.kframework.utils.Stopwatch;
import org.kframework.utils.file.FileUtil;


import java.io.File;

public class LatexBackend extends PosterBackend {

    private String latexFilePath;
    private boolean makeDocument = false;

    private final FileUtil files;
    private final KompileOptions options;

    LatexBackend(Stopwatch sw, KompileOptions options, FileUtil files) {
        super(sw);
        this.options = options;
        this.files = files;
    }

    public LatexBackend(Stopwatch sw, Context context, KompileOptions options, boolean doc, FileUtil files) {
        this(sw, options, files);
        makeDocument = doc;
    }

//    public void compile(Definition javaDef) {
//        String endl = System.getProperty("line.separator");
//
//        LatexFilter lf;
//        if(makeDocument) lf = new DocumentationFilter(context);
//        else lf = new LatexFilter(context);
//        lf.visitNode(javaDef);
//
//        files.saveToTemp("k.sty", files.loadFromKBase("include/latex/k.sty"));
//
//        String latexified = "\\nonstopmode" + endl +
//                "\\PassOptionsToPackage{pdftex,usenames,dvipsnames,svgnames,x11names}{xcolor}"+ endl +
//                "\\PassOptionsToPackage{pdftex}{hyperref}"+ endl +
//                "\\documentclass{article}" + endl + "\\usepackage[" + options.docStyle() + "]{k}" + endl;
//        String preamble = lf.getPreamble().toString();
//        latexified += preamble + "\\begin{document}" + endl + lf.getResult() + "\\end{document}" + endl;
//
//        File canonicalFile = options.outerParsing.mainDefinitionFile(files);
//        if(makeDocument) latexFilePath = FilenameUtils.removeExtension(canonicalFile.getName()) + "-doc.tex";
//        else latexFilePath = FilenameUtils.removeExtension(canonicalFile.getName()) + ".tex";
//        files.saveToTemp(latexFilePath, latexified);
//
//        sw.printIntermediate("Latex Generation");
//    }

    public String getLatexFile() {
        return latexFilePath;
    }

    @Override
    public void run() {
//            compile(javaDef);
            files.copyTempFileToDefinitionDirectory("k.sty");
            files.copyTempFileToDefinitionDirectory(latexFilePath);
    }
}
