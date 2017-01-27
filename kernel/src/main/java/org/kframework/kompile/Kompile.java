// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.kompile;

import org.kframework.KapiGlobal;
import org.kframework.Strategy;
import org.kframework.attributes.Source;
import org.kframework.backend.Backends;
import org.kframework.builtin.Sorts;
import org.kframework.compile.ConfigurationInfoFromModule;
import org.kframework.compile.LabelInfo;
import org.kframework.compile.LabelInfoFromModule;
import org.kframework.definition.Constructors;
import org.kframework.definition.Definition;
import org.kframework.definition.DefinitionTransformer;
import org.kframework.definition.Module;
import org.kframework.definition.Rule;
import org.kframework.definition.Sentence;
import org.kframework.kore.ADT;
import org.kframework.kore.K;
import org.kframework.kore.KApply;
import org.kframework.kore.KORE;
import org.kframework.kore.KSequence;
import org.kframework.kore.Sort;
import org.kframework.kore.TransformK;
import org.kframework.kore.compile.AddImplicitComputationCell;
import org.kframework.kore.compile.ConcretizeCells;
import org.kframework.kore.compile.GenerateSortPredicateSyntax;
import org.kframework.kore.compile.ResolveAnonVar;
import org.kframework.kore.compile.ConvertContextsToHeatCoolRules;
import org.kframework.kore.compile.ResolveFreshConstants;
import org.kframework.kore.compile.ResolveHeatCoolAttribute;
import org.kframework.kore.compile.ResolveIOStreams;
import org.kframework.kore.compile.ResolveSemanticCasts;
import org.kframework.kore.compile.ConvertStrictToContexts;
import org.kframework.kore.compile.SortInfo;
import org.kframework.kore.compile.checks.CheckConfigurationCells;
import org.kframework.kore.compile.checks.CheckRHSVariables;
import org.kframework.kore.compile.checks.CheckSortTopUniqueness;
import org.kframework.kore.compile.checks.CheckStreams;
import org.kframework.main.GlobalOptions;
import org.kframework.parser.concrete2kore.ParserUtils;
import org.kframework.parser.concrete2kore.generator.RuleGrammarGenerator;
import org.kframework.utils.Stopwatch;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;
import org.kframework.utils.file.JarInfo;
import scala.Option;

import java.io.File;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.kframework.Collections.*;
import static org.kframework.definition.Constructors.*;

/**
 * The new compilation pipeline. Everything is just wired together and will need clean-up once we deside on design.
 * Tracked by #1442.
 */
public class Kompile {
    public static final File BUILTIN_DIRECTORY = JarInfo.getKIncludeDir().resolve("builtin").toFile();
    public static final String REQUIRE_PRELUDE_K = "requires \"prelude.k\"\n";
    public final KompileOptions kompileOptions;
    private final FileUtil files;
    public final KExceptionManager kem;
    private final ParserUtils parser;
    private final Stopwatch sw;
    private final DefinitionParsing definitionParsing;
    java.util.Set<KEMException> errors;

    public Kompile(KompileOptions kompileOptions, FileUtil files, KExceptionManager kem, Stopwatch sw, boolean cacheParses) {
        this(kompileOptions, kompileOptions.global, files, kem, sw, cacheParses);
    }

    public Kompile(KompileOptions kompileOptions, FileUtil files, KExceptionManager kem, boolean cacheParses) {
        this(kompileOptions, files, kem, new Stopwatch(kompileOptions.global), cacheParses);
    }

    public Kompile(KompileOptions kompileOptions, FileUtil files, KExceptionManager kem) {
        this(kompileOptions, files, kem, true);
    }

    public Kompile(KompileOptions kompileOptions, FileUtil files, KExceptionManager kem, Stopwatch sw) {
        this(kompileOptions, files, kem, sw, true);
    }

    public Kompile(KompileOptions kompileOptions, GlobalOptions global, FileUtil files, KExceptionManager kem, Stopwatch sw, boolean cacheParses) {
        this.kompileOptions = kompileOptions;
        this.files = files;
        this.kem = kem;
        this.errors = new HashSet<>();
        this.parser = new ParserUtils(files::resolveWorkingDirectory, kem, global);
        List<File> lookupDirectories = kompileOptions.outerParsing.includes.stream().map(files::resolveWorkingDirectory).collect(Collectors.toList());
        this.definitionParsing = new DefinitionParsing(
                lookupDirectories, kompileOptions.strict(), kem,
                parser, cacheParses, files.resolveKompiled(FileUtil.CACHE_BIN), !kompileOptions.outerParsing.noPrelude);
        this.sw = sw;
    }

    public Kompile(KapiGlobal kapiGlobal) {
        this(kapiGlobal.kompileOptions, kapiGlobal.files, kapiGlobal.kem);
    }

    public CompiledDefinition run(File definitionFile, String mainModuleName, String mainProgramsModuleName) {
        return run(definitionFile, mainModuleName, mainProgramsModuleName, defaultSteps(kompileOptions, kem));
    }

    /**
     * Executes the Kompile tool. This tool first parses the definition and then compiles it.
     *
     * @param definitionFile
     * @param mainModuleName
     * @param mainProgramsModuleName
     * @param programStartSymbol
     * @return
     */
    public CompiledDefinition run(File definitionFile, String mainModuleName, String mainProgramsModuleName, Function<Definition, Definition> pipeline) {
        Definition parsedDef = parseDefinition(definitionFile, mainModuleName, mainProgramsModuleName);
        sw.printIntermediate("Parse definition [" + definitionParsing.parsedBubbles.get() + "/" + (definitionParsing.parsedBubbles.get() + definitionParsing.cachedBubbles.get()) + " rules]");

        return compile(parsedDef, pipeline);
    }

    public CompiledDefinition compile(Definition parsedDef, Function<Definition, Definition> pipeline) {
        checkDefinition(parsedDef);

        Definition kompiledDefinition = pipeline.apply(parsedDef);
        sw.printIntermediate("Apply compile pipeline");

        ConfigurationInfoFromModule configInfo = new ConfigurationInfoFromModule(kompiledDefinition.mainModule());

        return new CompiledDefinition(kompileOptions, parsedDef, kompiledDefinition, configInfo.getDefaultCell(configInfo.topCell()).klabel());
    }

    public Definition parseDefinition(File definitionFile, String mainModuleName, String mainProgramsModule) {
        return definitionParsing.parseDefinitionAndResolveBubbles(definitionFile, mainModuleName, mainProgramsModule);
    }

    public static Function<Definition, Definition> defaultSteps(KompileOptions kompileOptions, KExceptionManager kem) {

        return d -> {
            d = new ResolveIOStreams(d, kem).apply(d);
            d = new ConvertStrictToContexts(kompileOptions).apply(d);
            d = new ResolveAnonVar().apply(d);
            d = new ConvertContextsToHeatCoolRules(kompileOptions).resolve(d);
            d = new ResolveHeatCoolAttribute(new HashSet<>(kompileOptions.transition)).apply(d);
            d = new ResolveSemanticCasts(kompileOptions.backend.equals(Backends.JAVA)).apply(d);
            d = DefinitionTransformer.fromWithInputDefinitionTransformerClass(GenerateSortPredicateSyntax.class).apply(d);
            d = resolveFreshConstants(d);
            d = AddImplicitComputationCell.transformDefinition(d);
            d = new Strategy(kompileOptions.experimental.heatCoolStrategies).addStrategyCellToRulesTransformer().apply(d);
            d = ConcretizeCells.transformDefinition(d);
            d = addSemanticsModule(d);
            return d;
        };
    }

    public Rule parseAndCompileRule(CompiledDefinition compiledDef, String contents, Source source, Optional<Rule> parsedRule) {
        Rule parsed = parsedRule.orElse(parseRule(compiledDef, contents, source));
        return compileRule(compiledDef, parsed);
    }

    public Rule parseRule(CompiledDefinition compiledDef, String contents, Source source) {
        return definitionParsing.parseRule(compiledDef, contents, source);
    }

    private void checkDefinition(Definition parsedDef) {
        CheckRHSVariables checkRHSVariables = new CheckRHSVariables(errors);
        stream(parsedDef.modules()).forEach(m -> stream(m.localSentences()).forEach(checkRHSVariables::check));

        stream(parsedDef.modules()).forEach(m -> stream(m.localSentences()).forEach(new CheckConfigurationCells(errors, m)::check));

        stream(parsedDef.modules()).forEach(m -> stream(m.localSentences()).forEach(new CheckSortTopUniqueness(errors, m)::check));

        stream(parsedDef.modules()).forEach(m -> stream(m.localSentences()).forEach(new CheckStreams(errors, m)::check));

        if (!errors.isEmpty()) {
            kem.addAllKException(errors.stream().map(e -> e.exception).collect(Collectors.toList()));
            throw KEMException.compilerError("Had " + errors.size() + " structural errors.");
        }
    }

    public static Definition addSemanticsModule(Definition d) {
        java.util.Set<Sentence> prods = new HashSet<>();
        for (Sort srt : iterable(d.mainModule().definedSorts())) {
            if (!RuleGrammarGenerator.isParserSort(srt)) {
                // KItem ::= Sort
                prods.add(Production(Sorts.KItem(), Seq(NonTerminal(srt)), Att()));
            }
        }
        Module withKSeq = Constructors.Module("SEMANTICS", Set(d.mainModule()), immutable(prods), Att());
        java.util.Set<Module> allModules = mutable(d.modules());
        allModules.add(withKSeq);

        Module languageParsingModule = Constructors.Module("LANGUAGE-PARSING",
                Set(d.mainModule(),
                        d.getModule("K-TERM").get(),
                        d.getModule(RuleGrammarGenerator.ID_PROGRAM_PARSING).get()), Set(), Att());
        allModules.add(languageParsingModule);
        return Constructors.Definition(withKSeq, immutable(allModules), d.att());
    }

    public static Definition resolveFreshConstants(Definition input) {
        return DefinitionTransformer.fromHybrid(new ResolveFreshConstants(input)::resolve, "resolving !Var variables")
                .apply(input);
    }

    public Rule compileRule(CompiledDefinition compiledDef, Rule parsedRule) {
        return (Rule) asScalaFunc((Sentence s) -> new ResolveAnonVar().process(s))
                .andThen((Sentence s) ->  new ResolveSemanticCasts(kompileOptions.backend.equals(Backends.JAVA)).process(s))
                .andThen(s -> concretizeSentence(s, compiledDef.kompiledDefinition))
                .apply(parsedRule);
    }

    public Module parseModule(CompiledDefinition definition, File definitionFile) {
        return definitionParsing.parseModule(definition, definitionFile, !kompileOptions.outerParsing.noPrelude);
    }

    private Sentence concretizeSentence(Sentence s, Definition input) {
        ConfigurationInfoFromModule configInfo = new ConfigurationInfoFromModule(input.mainModule());
        LabelInfo labelInfo = new LabelInfoFromModule(input.mainModule());
        SortInfo sortInfo = SortInfo.fromModule(input.mainModule());
        return new ConcretizeCells(configInfo, labelInfo, sortInfo, input.mainModule()).concretize(s);
    }

    public static DefinitionTransformer moduleQualifySortPredicates = DefinitionTransformer.fromKTransformerWithModuleInfo(
            (Module m, K k) -> new TransformK() {
                @Override
                public K apply(KApply kk) {
                    KApply k = (KApply) super.apply(kk);
                    if (!k.klabel().name().startsWith("is"))
                        return k;

                    Sort possibleSort = KORE.Sort(k.klabel().name().substring("is".length()));
                    Option<ADT.Sort> resolvedSort = m.sortResolver().get(possibleSort);

                    if (resolvedSort.isDefined()) {
                        return KORE.KApply(KORE.KLabel("is" + resolvedSort.get().name()), k.klist());
                    } else {
                        return k;
                    }
                }
            }.apply(k), "Module-qualify sort predicates");

    /**
     * In the Java backend, {@link KSequence}s are treated like {@link KApply}s, so tranform them.
     */
    public static K convertKSeqToKApply(K ruleBody) {
        return new TransformK() {
            public K apply(KSequence kseq) {
                return super.apply(((ADT.KSequence) kseq).kApply());
            }
        }.apply(ruleBody);
    }
}
