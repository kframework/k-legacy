package org.kframework.backend.kore.latex;

import com.google.inject.Inject;
import com.sun.org.apache.xpath.internal.operations.Mod;
import org.apache.commons.io.FilenameUtils;
import org.kframework.definition.Module;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kompile.ConcreteDefinition;
import org.kframework.kompile.KompileOptions;
import org.kframework.kore.kdoc.PosterBackend;
import org.kframework.utils.file.FileUtil;

import java.io.File;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by Edgar Pek on 9/14/15.
 */
public class LatexBackend extends PosterBackend {

    private static final String STY_FILE = "k.sty";

    private String latexFilePath;
    private final KompileOptions options;
    private final FileUtil files;

    @Inject
    LatexBackend(KompileOptions kompileOptions, FileUtil files) {
        this.options = kompileOptions;
        this.files = files;
    }

    /***
     * compiles the definition created in kompile phase
     *
     ***/
    public void compile(CompiledDefinition compiledDefinition) {
        String endl = System.getProperty("line.separator");

        LatexFilter lf = new LatexFilter(this.options, compiledDefinition.getConcreteDefinition());
        lf.visitConcreteDefinition();

        files.saveToTemp(STY_FILE, files.loadFromKBase("include/latex/" + STY_FILE));

        String latexified = "\\nonstopmode" + endl +
                "\\PassOptionsToPackage{pdftex,usenames,dvipsnames,svgnames,x11names}{xcolor}"+ endl +
                "\\PassOptionsToPackage{pdftex}{hyperref}"+ endl +
                "\\documentclass{article}" + endl + "\\usepackage[" + options.docStyle() + "]{k}" + endl;

        String preamble = lf.getPreamble().toString();
        latexified += preamble + "\\begin{document}" + endl + lf.getResult() + "\\end{document}" + endl;

        File canonicalFile = options.mainDefinitionFile();
        latexFilePath = FilenameUtils.removeExtension(canonicalFile.getName()) + ".tex";
        files.saveToTemp(latexFilePath, latexified);

    }

    public String getLatexFileName() { return latexFilePath; }

    @Override
    public void run(CompiledDefinition definition) {
        System.out.println("Latex backend");
        this.compile(definition);
        this.files.copyTempFileToDefinitionDirectory(STY_FILE);
        this.files.copyTempFileToDefinitionDirectory(latexFilePath);
    }
}
