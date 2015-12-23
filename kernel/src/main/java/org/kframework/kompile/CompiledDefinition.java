// Copyright (c) 2015 K Team. All Rights Reserved.
package org.kframework.kompile;

import org.kframework.attributes.Att;
import org.kframework.attributes.Source;
import org.kframework.definition.Definition;
import org.kframework.definition.Module;
import org.kframework.definition.Rule;
import org.kframework.kore.K;
import org.kframework.kore.KLabel;
import org.kframework.kore.Sort;
import org.kframework.parser.TreeNodesToKORE;
import org.kframework.parser.concrete2kore.ParseInModule;
import org.kframework.parser.concrete2kore.generator.RuleGrammarGenerator;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.errorsystem.ParseFailedException;
import org.kframework.utils.file.FileUtil;
import scala.Option;
import scala.Tuple2;
import scala.util.Either;

import java.io.IOException;
import java.io.Serializable;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

/**
 * A class representing a compiled definition. It has everything needed for executing and parsing programs.
 */

public class CompiledDefinition implements Serializable {
    public final KompileOptions kompileOptions;
    private final Definition parsedDefinition;
    public final Definition kompiledDefinition;
    public final Sort programStartSymbol;
    public final KLabel topCellInitializer;
    private final Module languageParsingModule;
    private transient Map<String, Rule> cachedcompiledPatterns;
    private transient Map<String, Rule> cachedParsedPatterns;


    public CompiledDefinition(KompileOptions kompileOptions, Definition parsedDefinition, Definition kompiledDefinition, Sort programStartSymbol, KLabel topCellInitializer) {
        this.kompileOptions = kompileOptions;
        this.parsedDefinition = parsedDefinition;
        this.kompiledDefinition = kompiledDefinition;
        this.programStartSymbol = programStartSymbol;
        this.topCellInitializer = topCellInitializer;
        this.languageParsingModule = kompiledDefinition.getModule("LANGUAGE-PARSING").get();
    }

    /**
     * A function that takes a string and the source of that string and parses it as a program into KAST.
     */
    public BiFunction<String, Source, K> getProgramParser(KExceptionManager kem) {
        return getParser(generatedProgramParsingModuleFor(mainSyntaxModuleName()).get(), programStartSymbol, kem);
    }

    /**
     * The parsed but uncompiled definition
     */
    public Definition getParsedDefinition() {
        return parsedDefinition;
    }

    /**
     * A module containing the compiled definition
     */
    public Module executionModule() {
        return kompiledDefinition.mainModule();
    }

    public String mainSyntaxModuleName() { return parsedDefinition.att().<String>getOptional(Att.syntaxModule()).get(); }

    public Option<Module> generatedProgramParsingModuleFor(String moduleName) {
        RuleGrammarGenerator gen = new RuleGrammarGenerator(parsedDefinition, kompileOptions.strict());

        Option<Module> userProgramParsingModule = parsedDefinition.getModule(moduleName + RuleGrammarGenerator.POSTFIX);
        if(userProgramParsingModule.isDefined()) {
            return userProgramParsingModule;
        } else {
            Option<Module> moduleOption = parsedDefinition.getModule(moduleName);
            return moduleOption.isDefined() ? Option.apply(gen.getProgramsGrammar(moduleOption.get())) : Option.empty();
        }
    }

    public Module languageParsingModule() { return languageParsingModule; }

    /**
     * Creates a parser for a module.
     * Will probably want to move the method out of this class here eventually.
     *
     * @return a function taking a String to be parsed, a Source, and returning the parsed string as K.
     */

    public BiFunction<String, Source, K> getParser(Module module, Sort programStartSymbol, KExceptionManager kem) {
        ParseInModule parseInModule = new RuleGrammarGenerator(parsedDefinition, kompileOptions.strict()).getCombinedGrammar(module);

        return (BiFunction<String, Source, K> & Serializable) (s, source) -> {
            Tuple2<Either<Set<ParseFailedException>, K>, Set<ParseFailedException>> res = parseInModule.parseString(s, programStartSymbol, source);
            kem.addAllKException(res._2().stream().map(e -> e.getKException()).collect(Collectors.toSet()));
            if (res._1().isLeft()) {
                throw res._1().left().get().iterator().next();
            }
            return TreeNodesToKORE.down(res._1().right().get());
        };
    }

    public Module getExtensionModule(Module module) {
        return new RuleGrammarGenerator(kompiledDefinition, kompileOptions.strict()).getCombinedGrammar(module).getExtensionModule();
    }

    public Rule compilePatternIfAbsent(FileUtil files, KExceptionManager kem, String pattern, Source source) {
        return cachedcompiledPatterns.computeIfAbsent(pattern, p -> new Kompile(kompileOptions, files, kem).parseAndCompileRule(this, p, source,
                Optional.of(parsePatternIfAbsent(files, kem, pattern, source))));
    }

    public Rule parsePatternIfAbsent(FileUtil files, KExceptionManager kem, String pattern, Source source) {
        return cachedParsedPatterns.computeIfAbsent(pattern, p -> new Kompile(kompileOptions, files, kem).parseRule(this, p, source));
    }

    private void readObject(java.io.ObjectInputStream stream)
         throws IOException, ClassNotFoundException {
        stream.defaultReadObject();
        cachedcompiledPatterns = new ConcurrentHashMap<>();
        cachedParsedPatterns = new ConcurrentHashMap<>();
    }
}
