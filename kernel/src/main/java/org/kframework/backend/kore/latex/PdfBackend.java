package org.kframework.backend.kore.latex;

import com.google.inject.Inject;
import com.google.inject.Provider;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kompile.ConcreteDefinition;
import org.kframework.kore.kdoc.PosterBackend;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.file.FileUtil;

import java.io.File;
import java.io.IOException;

/**
 * Created by Edgar Pek on 9/14/15.
 */
public class PdfBackend extends PosterBackend {

    private final LatexBackend latexBackend;
    private final FileUtil files;
    private final Provider<ProcessBuilder> pb;

    @Inject
    PdfBackend(
            LatexBackend latexBackend,
            FileUtil files,
            Provider<ProcessBuilder> pb) {
        this.latexBackend = latexBackend;
        this.files = files;
        this.pb = pb;
    }

    // copied from the old PdfBackend
    private String generatePdf(File latexFile) {
        try {
            // Run pdflatex.
            String pdfLatex = "pdflatex";
            String argument = latexFile.getCanonicalPath();

            ProcessBuilder pb = this.pb.get().command(
                    pdfLatex, argument, "-interaction", "nonstopmode");
            pb.redirectErrorStream(true);
            pb.directory(latexFile.getParentFile());

            Process process = pb.start();
            IOUtils.toString(process.getInputStream());
            process.waitFor();
            if (process.exitValue() != 0) {
                String latexLogFile = FilenameUtils.removeExtension(latexFile.getName()) + ".log";
                files.copyTempFileToDefinitionDirectory(latexLogFile);
                throw KEMException.criticalError("pdflatex returned a non-zero exit code. " +
                        "The pdf might be generated, but with bugs. " +
                        "Please inspect the latex logs.");
            }

            return FilenameUtils.removeExtension(latexFile.getName()) + ".pdf";
        } catch (IOException | InterruptedException e) {
            throw KEMException.criticalError(
                    "Cannot generate the pdf version of the definition. " +
                            "It seems that `pdflatex` is not installed or is not in your path. " +
                            "To generate the pdf version you can run `pdflatex` having as " +
                            "argument the latex version of the definition.", e);
        }
    }

    @Override
    public void run(CompiledDefinition definition) {
        // TODO
        System.out.println("In Pdf backend");
        latexBackend.compile(definition);
        String latexFileName = latexBackend.getLatexFileName();
        String pdfFileName = generatePdf(files.resolveTemp(latexFileName));
        files.copyTempFileToDefinitionDirectory(pdfFileName);
    }
}
