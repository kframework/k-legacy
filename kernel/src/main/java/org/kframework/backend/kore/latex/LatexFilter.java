package org.kframework.backend.kore.latex;

import org.kframework.backend.kore.BackendFilter;
import org.kframework.definition.Definition;
import org.kframework.definition.Production;
import org.kframework.definition.Sentence;
import org.kframework.kil.DefinitionItem;
import org.kframework.kil.LiterateComment;
import org.kframework.kil.LiterateDefinitionComment;
import org.kframework.kil.LiterateModuleComment;
import org.kframework.kompile.ConcreteDefinition;
import org.kframework.kompile.ConcreteModules;
import org.kframework.kompile.ConcreteSentences;
import org.kframework.kompile.KompileOptions;
import org.kframework.krun.api.io.File;
import org.kframework.utils.StringUtil;
import org.kframework.utils.file.FileUtil;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

/**
 * Created by Edgar Pek on 9/23/15.
 */
public class LatexFilter extends BackendFilter {

    public LatexFilter(KompileOptions options, Definition concreteDefinition, String indent) {
        super(options);
        this.concreteDefinition = concreteDefinition;
        this.latexifyDefinition = new LatexifyDefinition(concreteDefinition, indent);
    }
    public LatexFilter(KompileOptions options, Definition concreteDefinition) {
        this(options, concreteDefinition, "");
    }

    private final Definition concreteDefinition;

    private LatexifyDefinition latexifyDefinition;
    //private LatexPatterns latexPatterns;
    //private LatexifySentence latexifySentence = new LatexifySentence(this.endl);


    private StringBuilder preamble;
    public StringBuilder getPreamble() { return this.preamble; };
    // public void increaseIndent() { this.indent += "  "; }
    // public void decreaseIndent() { this.indent = this.indent.substring(2); }
    // private void newLine() { this.result.append(endl).append(indent); }

    public void visitConcreteDefinition() {
        this.latexifyDefinition.latexifyDefinition();
        this.result = new StringBuilder(this.latexifyDefinition.result());
        this.preamble = new StringBuilder(this.latexifyDefinition.preamble());


    /*
        result.append("\\begin{kdefinition}" + endl + "\\maketitle" + endl);
        LinkedList<DefinitionItem> outerSyntaxList = this.concreteDefinition.getOuterKILSyntax();
        System.out.println("traversing the concrete definition list:");
        int count = 0;
        for(DefinitionItem di : outerSyntaxList) {
            System.out.print("Item #" + count + " ");
            this.visitNode(di);
            count ++;
        }
        result.append("\\end{kdefinition}" + endl);
        if (!hasTitle) {
            preamble.append("\\title{" + options.mainModule() + "}" + endl);
            hasTitle = true;
        }
    */
    }

    /*
    @Override
    public Void visit(org.kframework.kil.Require require, Void _void) {
        System.out.println("Visiting Requires " + require.getValue());
        return null;
    }


    @Override
    public Void visit(DefinitionItem di, Void _void) {
        System.out.println("Visiting definition item -- TODO: refine this ");
        super.visit(di, _void);
        return null;
    }
    */

    @Override
    public Void visit(org.kframework.kil.Module module, Void _void) {
        String moduleName = module.getName();
        System.out.println("************ Visiting module " + moduleName + " **************** ");
        /* FIXME:

        ConcreteModules concreteModules = this.concreteDefinition.getTopLevelModules();
        ConcreteSentences concreteSentences = concreteModules.getConcreteSentences(moduleName);
        if (!concreteSentences.iterator().hasNext()) {
            return null;
        }
        result.append("\\begin{module}{\\moduleName{" + StringUtil.latexify(module.getName()) + "}}" + endl);
        for(Sentence s: concreteSentences) {
            // FIXME: instanceof ugliness?
            if (s instanceof Production) {
                String prodName = ((Production) s).sort().name();
                if (!prodName.contentEquals(this.curProductionSortName)) {
                    this.endSyntaxBlockIfPendingProduction();
                    this.curProductionSortName = prodName;
                    result.append(endl + "\\begin{syntaxBlock}");
                    result.append("{\\nonTerminal{\\sort{" + StringUtil.latexify(prodName) + "}}}");
                    increaseIndent();
               }
            } else {
                this.endSyntaxBlockIfPendingProduction();
                this.curProductionSortName = "";
            }
            this.latexifySentence.setIndent(this.indent);
            this.latexifySentence.latexify(s);
            this.indent = this.latexifySentence.indent();
            this.result.append(this.latexifySentence.result());
            this.latexifySentence.clearResult();
        }
        this.endSyntaxBlockIfPendingProduction();
        this.curProductionSortName = "";
        result.append("\\end{module}" + endl);
        */
        System.out.println("************ [DONE " + moduleName + "] **************** ");

        return null;
    }

    /*
    private void endSyntaxBlockIfPendingProduction() {
        if (!curProductionSortName.contentEquals("")) {
            this.latexifySentence.resetFirstProduction();
            result.append(endl + "\\end{syntaxBlock}");
            decreaseIndent();
            newLine();
            result.append("%");
            newLine();
        }
    }
    */

    @Override
    public Void visit(LiterateDefinitionComment comment, Void _void) {
        /*
        System.out.println("Visiting LiterateDefinitionComment: " + comment.getValue());
        if (comment.getType() == LiterateComment.LiterateCommentType.LATEX) {
            this.result.append("\\begin{kblock}[text]" + this.endl);
            this.result.append(comment.getValue());
            this.result.append(this.endl + "\\end{kblock}" + this.endl);
            this.result.append("%");
            newLine();
        } else if (comment.getType() == LiterateComment.LiterateCommentType.PREAMBLE) {
            this.preamble.append(comment.getValue());
            if (comment.getValue().contains("\\title{")) {
                this.hasTitle = true;
            }
        }
        */
        return null;
    }

    @Override
    public Void visit(LiterateModuleComment comment, Void _void) {
        /*
        System.out.println("Visiting LiterateModuleComment: " + comment.getValue());
        if (comment.getType() == LiterateComment.LiterateCommentType.LATEX) {
            result.append("\\begin{kblock}[text]" + endl);
            result.append(comment.getValue());
            result.append(endl + "\\end{kblock}" + endl);
            result.append("%");
            newLine();
        } else if (comment.getType() == LiterateComment.LiterateCommentType.PREAMBLE) {
            preamble.append(comment.getValue());
            if (comment.getValue().contains("\\title{")) {
                hasTitle = true;
            }
        }
        */
        return null;
    }




}
