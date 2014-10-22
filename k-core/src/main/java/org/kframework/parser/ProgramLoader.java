// Copyright (c) 2012-2014 K Team. All Rights Reserved.
package org.kframework.parser;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.List;

import org.kframework.compile.transformers.AddEmptyLists;
import org.kframework.compile.transformers.FlattenTerms;
import org.kframework.compile.transformers.RemoveBrackets;
import org.kframework.compile.transformers.RemoveSyntacticCasts;
import org.kframework.compile.utils.CompilerStepDone;
import org.kframework.compile.utils.RuleCompilerSteps;
import org.kframework.kil.ASTNode;
import org.kframework.kil.Definition;
import org.kframework.kil.DefinitionItem;
import org.kframework.kil.Location;
import org.kframework.kil.Rule;
import org.kframework.kil.Sentence;
import org.kframework.kil.Sort;
import org.kframework.kil.Source;
import org.kframework.kil.Term;
import org.kframework.kil.loader.Context;
import org.kframework.kil.loader.JavaClassesFactory;
import org.kframework.kil.loader.ResolveVariableAttribute;
import org.kframework.main.GlobalOptions;
import org.kframework.parser.concrete.disambiguate.AmbDuplicateFilter;
import org.kframework.parser.concrete.disambiguate.AmbFilter;
import org.kframework.parser.concrete.disambiguate.BestFitFilter;
import org.kframework.parser.concrete.disambiguate.CellEndLabelFilter;
import org.kframework.parser.concrete.disambiguate.CellTypesFilter;
import org.kframework.parser.concrete.disambiguate.CorrectCastPriorityFilter;
import org.kframework.parser.concrete.disambiguate.CorrectKSeqFilter;
import org.kframework.parser.concrete.disambiguate.CorrectRewritePriorityFilter;
import org.kframework.parser.concrete.disambiguate.FlattenListsFilter;
import org.kframework.parser.concrete.disambiguate.GetFitnessUnitKCheckVisitor;
import org.kframework.parser.concrete.disambiguate.GetFitnessUnitTypeCheckVisitor;
import org.kframework.parser.concrete.disambiguate.NormalizeASTTransformer;
import org.kframework.parser.concrete.disambiguate.PreferAvoidFilter;
import org.kframework.parser.concrete.disambiguate.PreferDotsFilter;
import org.kframework.parser.concrete.disambiguate.PriorityFilter;
import org.kframework.parser.concrete.disambiguate.SentenceVariablesFilter;
import org.kframework.parser.concrete.disambiguate.TypeInferenceSupremumFilter;
import org.kframework.parser.concrete.disambiguate.TypeSystemFilter;
import org.kframework.parser.concrete.disambiguate.TypeSystemFilter2;
import org.kframework.parser.concrete.disambiguate.VariableTypeInferenceFilter;
import org.kframework.parser.concrete2.Grammar;
import org.kframework.parser.concrete2.MakeConsList;
import org.kframework.parser.concrete2.Parser;
import org.kframework.parser.concrete2.Parser.ParseError;
import org.kframework.parser.concrete2.TreeCleanerVisitor;
import org.kframework.parser.generator.DisambiguateRulesFilter;
import org.kframework.parser.generator.ParseConfigsFilter;
import org.kframework.parser.generator.ParseRulesFilter;
import org.kframework.parser.outer.Outer;
import org.kframework.utils.BinaryLoader;
import org.kframework.utils.Stopwatch;
import org.kframework.utils.XmlLoader;
import org.kframework.utils.errorsystem.KException;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.errorsystem.ParseFailedException;
import org.kframework.utils.errorsystem.KException.ExceptionType;
import org.kframework.utils.errorsystem.KException.KExceptionGroup;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.google.inject.Inject;
import com.sun.org.apache.xerces.internal.impl.dv.util.Base64;

public class ProgramLoader {

    private final BinaryLoader loader;
    private final Stopwatch sw;
    private final KExceptionManager kem;
    private final GlobalOptions globalOptions;

    @Inject
    ProgramLoader(
            BinaryLoader loader,
            Stopwatch sw,
            KExceptionManager kem,
            GlobalOptions globalOptions) {
        this.loader = loader;
        this.sw = sw;
        this.kem = kem;
        this.globalOptions = globalOptions;
    }

    /**
     * Load program file to ASTNode.
     *
     * @param kappize
     *            If true, then apply KAppModifier to AST.
     */
    public ASTNode loadPgmAst(String content, Source source, Boolean kappize, Sort startSymbol, Context context)
            throws ParseFailedException {
        // ------------------------------------- import files in Stratego
        ASTNode out;

        org.kframework.parser.concrete.KParser.ImportTblPgm(context.files.resolveKompiled("."));
        String parsed = org.kframework.parser.concrete.KParser.ParseProgramString(content, startSymbol.toString());
        Document doc = XmlLoader.getXMLDoc(parsed);

        XmlLoader.addSource(doc.getFirstChild(), source);
        XmlLoader.reportErrors(doc);
        JavaClassesFactory.startConstruction(context);
        out = JavaClassesFactory.getTerm((Element) doc.getDocumentElement().getFirstChild().getNextSibling());
        JavaClassesFactory.endConstruction();

        out = new PriorityFilter(context).visitNode(out);
        out = new PreferAvoidFilter(context).visitNode(out);
        out = new NormalizeASTTransformer(context, kem).visitNode(out);
        out = new AmbFilter(context, kem).visitNode(out);
        out = new RemoveBrackets(context).visitNode(out);

        if (kappize)
            out = new FlattenTerms(context).visitNode(out);

        return out;
    }

    public ASTNode loadPgmAst(String content, Source source, Sort startSymbol, Context context) throws ParseFailedException {
        return loadPgmAst(content, source, true, startSymbol, context);
    }


    /**
     * Parses a string representing a file with modules in it. Returns the complete parse tree. Any bubble rule has been parsed and disambiguated.
     *
     * @param content
     *            - the input string.
     * @param source
     *            - only for error reporting purposes. Can be empty string.
     * @param context
     *            - the context for disambiguation purposes.
     * @return A lightweight Definition element which contain all the definition items found in the string.
     */
    public Definition parseString(String content, Source source, Context context) throws ParseFailedException {
        List<DefinitionItem> di = Outer.parse(source, content, context);

        org.kframework.kil.Definition def = new org.kframework.kil.Definition();
        def.setItems(di);

        // ------------------------------------- import files in Stratego
        org.kframework.parser.concrete.KParser.ImportTblRule(context.files.resolveKompiled("."));

        // ------------------------------------- parse configs
        JavaClassesFactory.startConstruction(context);
        def = (Definition) new ParseConfigsFilter(context, false, kem).visitNode(def);
        JavaClassesFactory.endConstruction();

        // ----------------------------------- parse rules
        JavaClassesFactory.startConstruction(context);
        def = (Definition) new ParseRulesFilter(context).visitNode(def);
        def = (Definition) new DisambiguateRulesFilter(context, false, kem).visitNode(def);
        def = (Definition) new NormalizeASTTransformer(context, kem).visitNode(def);

        JavaClassesFactory.endConstruction();

        return def;
    }

    public Term parseCmdString(String content, Source source, Sort startSymbol, Context context) throws ParseFailedException {
        if (!context.initialized) {
            assert false : "You need to load the definition before you call parsePattern!";
        }
        String parsed = org.kframework.parser.concrete.KParser.ParseKCmdString(content);
        Document doc = XmlLoader.getXMLDoc(parsed);
        XmlLoader.addSource(doc.getFirstChild(), source);
        XmlLoader.reportErrors(doc);

        JavaClassesFactory.startConstruction(context);
        org.kframework.kil.ASTNode config = JavaClassesFactory.getTerm((Element) doc.getFirstChild().getFirstChild().getNextSibling());
        JavaClassesFactory.endConstruction();

        // TODO: reject rewrites
        config = new SentenceVariablesFilter(context).visitNode(config);
        config = new CellEndLabelFilter(context).visitNode(config);
        //if (checkInclusion)
        //    config = new InclusionFilter(localModule, context).visitNode(config);
        config = new TypeSystemFilter2(startSymbol, context).visitNode(config);
        config = new CellTypesFilter(context).visitNode(config);
        config = new CorrectRewritePriorityFilter(context).visitNode(config);
        config = new CorrectKSeqFilter(context).visitNode(config);
        config = new CorrectCastPriorityFilter(context).visitNode(config);
        // config = new CheckBinaryPrecedenceFilter().visitNode(config);
        config = new PriorityFilter(context).visitNode(config);
        config = new PreferDotsFilter(context).visitNode(config);
        config = new VariableTypeInferenceFilter(context, kem).visitNode(config);
        config = new TypeSystemFilter(context).visitNode(config);
        config = new TypeInferenceSupremumFilter(context).visitNode(config);
        // config = new AmbDuplicateFilter(context).visitNode(config);
        // config = new TypeSystemFilter(context).visitNode(config);
        // config = new BestFitFilter(new GetFitnessUnitTypeCheckVisitor(context), context).visitNode(config);
        // config = new TypeInferenceSupremumFilter(context).visitNode(config);
        config = new BestFitFilter(new GetFitnessUnitKCheckVisitor(context), context).visitNode(config);
        config = new PreferAvoidFilter(context).visitNode(config);
        config = new NormalizeASTTransformer(context, kem).visitNode(config);
        config = new FlattenListsFilter(context).visitNode(config);
        config = new AmbDuplicateFilter(context).visitNode(config);
        // last resort disambiguation
        config = new AmbFilter(context, kem).visitNode(config);

        return (Term) config;
    }

    public ASTNode parsePattern(String pattern, Source source, Sort startSymbol, Context context) throws ParseFailedException {
        if (!context.initialized) {
            assert false : "You need to load the definition before you call parsePattern!";
        }

        String parsed = org.kframework.parser.concrete.KParser.ParseKRuleString(pattern);
        Document doc = XmlLoader.getXMLDoc(parsed);

        XmlLoader.addSource(doc.getFirstChild(), source);
        XmlLoader.reportErrors(doc);

        JavaClassesFactory.startConstruction(context);
        ASTNode config = JavaClassesFactory.getTerm((Element) doc.getDocumentElement().getFirstChild().getNextSibling());
        JavaClassesFactory.endConstruction();

        // TODO: reject rewrites
        config = new SentenceVariablesFilter(context).visitNode(config);
        config = new CellEndLabelFilter(context).visitNode(config);
        //if (checkInclusion)
        //    config = new InclusionFilter(localModule, context).visitNode(config);
        config = new TypeSystemFilter2(startSymbol, context).visitNode(config);
        config = new CellTypesFilter(context).visitNode(config);
        config = new CorrectRewritePriorityFilter(context).visitNode(config);
        config = new CorrectKSeqFilter(context).visitNode(config);
        config = new CorrectCastPriorityFilter(context).visitNode(config);
        // config = new CheckBinaryPrecedenceFilter().visitNode(config);
        config = new PriorityFilter(context).visitNode(config);
        config = new PreferDotsFilter(context).visitNode(config);
        config = new VariableTypeInferenceFilter(context, kem).visitNode(config);
        config = new TypeSystemFilter(context).visitNode(config);
        config = new TypeInferenceSupremumFilter(context).visitNode(config);
        // config = new AmbDuplicateFilter(context).visitNode(config);
        // config = new TypeSystemFilter(context).visitNode(config);
        // config = new BestFitFilter(new GetFitnessUnitTypeCheckVisitor(context), context).visitNode(config);
        // config = new TypeInferenceSupremumFilter(context).visitNode(config);
        config = new BestFitFilter(new GetFitnessUnitKCheckVisitor(context), context).visitNode(config);
        config = new PreferAvoidFilter(context).visitNode(config);
        config = new NormalizeASTTransformer(context, kem).visitNode(config);
        config = new FlattenListsFilter(context).visitNode(config);
        config = new AmbDuplicateFilter(context).visitNode(config);
        // last resort disambiguation
        config = new AmbFilter(context, kem).visitNode(config);

        return config;
    }

    public ASTNode parsePatternAmbiguous(String pattern, Context context) throws ParseFailedException {
        if (!context.initialized) {
            assert false : "You need to load the definition before you call parsePattern!";
        }

        String parsed = org.kframework.parser.concrete.KParser.ParseKRuleString(pattern);
        Document doc = XmlLoader.getXMLDoc(parsed);

        // XmlLoader.addFilename(doc.getFirstChild(), filename);
        XmlLoader.reportErrors(doc);

        JavaClassesFactory.startConstruction(context);
        ASTNode config = JavaClassesFactory.getTerm((Element) doc.getDocumentElement().getFirstChild().getNextSibling());
        JavaClassesFactory.endConstruction();

        // TODO: don't allow rewrites
        config = new SentenceVariablesFilter(context).visitNode(config);
        config = new CellEndLabelFilter(context).visitNode(config);
        config = new CellTypesFilter(context).visitNode(config);
        // config = new CorrectRewritePriorityFilter().visitNode(config);
        config = new CorrectKSeqFilter(context).visitNode(config);
        config = new CorrectCastPriorityFilter(context).visitNode(config);
        // config = new CheckBinaryPrecedenceFilter().visitNode(config);
        // config = new InclusionFilter(localModule).visitNode(config);
        // config = new VariableTypeInferenceFilter().visitNode(config);
        config = new AmbDuplicateFilter(context).visitNode(config);
        config = new TypeSystemFilter(context).visitNode(config);
        config = new PreferDotsFilter(context).visitNode(config);
        config = new VariableTypeInferenceFilter(context, kem).visitNode(config);
        // config = new PriorityFilter().visitNode(config);
        config = new BestFitFilter(new GetFitnessUnitTypeCheckVisitor(context), context).visitNode(config);
        config = new TypeInferenceSupremumFilter(context).visitNode(config);
        config = new BestFitFilter(new GetFitnessUnitKCheckVisitor(context), context).visitNode(config);
        // config = new PreferAvoidFilter().visitNode(config);
        config = new NormalizeASTTransformer(context, kem).visitNode(config);
        config = new FlattenListsFilter(context).visitNode(config);
        config = new AmbDuplicateFilter(context).visitNode(config);
        // last resort disambiguation
        // config = new AmbFilter().visitNode(config);
        return config;
    }

    /**
     * Print maudified program to standard output.
     *
     * Save it in kompiled cache under pgm.maude.
     */
    public Term processPgm(String content, Source source, Sort startSymbol,
            Context context, ParserType whatParser) throws ParseFailedException {
        sw.printIntermediate("Importing Files");
        if (!context.definedSorts.contains(startSymbol)) {
            throw new ParseFailedException(new KException(ExceptionType.ERROR, KExceptionGroup.CRITICAL,
                    "The start symbol must be declared in the definition. Found: " + startSymbol));
        }

        ASTNode out;
        if (whatParser == ParserType.GROUND) {
            org.kframework.parser.concrete.KParser.ImportTblGround(context.files.resolveKompiled("."));
            out = parseCmdString(new String(content), source, startSymbol, context);
            out = new RemoveBrackets(context).visitNode(out);
            out = new AddEmptyLists(context, kem).visitNode(out);
            out = new RemoveSyntacticCasts(context).visitNode(out);
            out = new FlattenTerms(context).visitNode(out);
        } else if (whatParser == ParserType.RULES) {
            org.kframework.parser.concrete.KParser.ImportTblRule(context.files.resolveKompiled("."));
            out = parsePattern(new String(content), source, startSymbol, context);
            out = new RemoveBrackets(context).visitNode(out);
            out = new AddEmptyLists(context, kem).visitNode(out);
            out = new RemoveSyntacticCasts(context).visitNode(out);
            try {
                out = new RuleCompilerSteps(context, kem).compile(
                        new Rule((Sentence) out),
                        null);
            } catch (CompilerStepDone e) {
                out = (ASTNode) e.getResult();
            }
            out = ((Rule) out).getBody();
        } else if (whatParser == ParserType.BINARY) {
            try (ByteArrayInputStream in = new ByteArrayInputStream(Base64.decode(content))) {
                out = loader.loadOrDie(Term.class, in);
            } catch (IOException e) {
                throw KExceptionManager.internalError("Error reading from binary file", e);
            }
        } else if (whatParser == ParserType.NEWPROGRAM) {
            // load the new parser
            // TODO(Radu): after the parser is in a good enough shape, replace the program parser
            // TODO(Radu): (the default one) with this branch of the 'if'
            Grammar grammar = loader.loadOrDie(Grammar.class, context.files.resolveKompiled("newParser.bin"));

            String contentString = new String(content);
            Parser parser = new Parser(contentString);
            out = parser.parse(grammar.get(startSymbol.toString()), 0);
            if (globalOptions.debug)
                System.err.println("Raw: " + out + "\n");
            try {
                out = new TreeCleanerVisitor(context).visitNode(out);
                out = new MakeConsList(context).visitNode(out);
                if (globalOptions.debug)
                    System.err.println("Clean: " + out + "\n");
                out = new PriorityFilter(context).visitNode(out);
                out = new PreferAvoidFilter(context).visitNode(out);
                if (globalOptions.debug)
                    System.err.println("Filtered: " + out + "\n");
                out = new AmbFilter(context, kem).visitNode(out);
                out = new RemoveBrackets(context).visitNode(out);
                out = new FlattenTerms(context).visitNode(out);
            } catch (ParseFailedException te) {
                ParseError perror = parser.getErrors();

                String msg = contentString.length() == perror.position ?
                    "Parse error: unexpected end of file." :
                    "Parse error: unexpected character '" + contentString.charAt(perror.position) + "'.";
                Location loc = new Location(perror.line, perror.column,
                                            perror.line, perror.column + 1);
                throw new ParseFailedException(new KException(
                        ExceptionType.ERROR, KExceptionGroup.INNER_PARSER, msg, source, loc));
            }
            out = new ResolveVariableAttribute(context).visitNode(out);
        } else {
            out = loadPgmAst(new String(content), source, startSymbol, context);
            out = new ResolveVariableAttribute(context).visitNode(out);
        }
        sw.printIntermediate("Parsing Program");

        return (Term) out;
    }
}
