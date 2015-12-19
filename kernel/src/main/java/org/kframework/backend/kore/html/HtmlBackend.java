package org.kframework.backend.kore.html;

import com.google.inject.Inject;
import org.apache.commons.io.FilenameUtils;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kompile.ConcreteDefinition;
import org.kframework.kore.kdoc.PosterBackend;
import org.kframework.utils.file.FileUtil;

/**
 * Created by Edgar Pek on 9/14/15.
 */
public class HtmlBackend extends PosterBackend {

    private static final String CSS_STYLE_FILE = "k-definition.css";
    private final FileUtil files;

    @Inject
    HtmlBackend(FileUtil files) { this.files = files; }

    @Override
    public void run(CompiledDefinition definition) {
        System.out.println("HTML backend.");
        // TODO: implement html visitor over concrete syntax
        // -- HTMLFilter htmlFilter = new HTMLFilter();
        // -- htmlFilter.visitNode(definition);
        // -- String html = htmlFilter.getHTML();
        //FIXME: definition.executionModule().att().get("name")
        //- files.saveToDefinitionDirectory(FilenameUtils.removeExtension(definition.getMainFile().getName()) + ".html", html);
        // -files.saveToDefinitionDirectory(CSS_STYLE_FILE, files.loadFromKBase("include/html/" + CSS_STYLE_FILE));
    }

}

