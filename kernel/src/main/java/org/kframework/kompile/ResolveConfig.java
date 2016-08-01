// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.kompile;

import org.kframework.Collections;
import org.kframework.Parser;
import org.kframework.attributes.Source;
import org.kframework.builtin.BooleanUtils;
import org.kframework.definition.Bubble;
import org.kframework.definition.Definition;
import org.kframework.definition.Module;
import org.kframework.definition.ModuleName;
import org.kframework.definition.Sentence;
import org.kframework.kore.K;
import org.kframework.kore.KApply;
import org.kframework.kore.compile.GenerateSentencesFromConfigDecl;
import org.kframework.parser.TreeNodesToKORE;
import org.kframework.parser.concrete2kore.ParseCache;
import org.kframework.parser.concrete2kore.ParseInModule;
import org.kframework.parser.concrete2kore.generator.RuleGrammarGenerator;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.ParseFailedException;
import org.scalameter.Keys;
import scala.Tuple2;
import scala.util.Either;
import scala.util.Left;
import scala.util.Right;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

import static org.kframework.Collections.*;
import static org.kframework.definition.Constructors.*;
import static org.kframework.kore.KORE.Sort;

/**
 * Expands configuration declaration to KORE productions and rules.
 */
public class ResolveConfig implements UnaryOperator<Module> {
    private final Definition def;
    private final boolean isStrict;
    private final BiFunction<Module, Bubble, Either<Set<ParseFailedException>, K>> parseBubble;
    private final RuleGrammarGenerator gen;
    private Function<Module, ParseInModule> getParser;

    private ParseInModule simpleGetParser(Module module) {
        return gen.getCombinedGrammar(gen.getConfigGrammar(module));
    }

    private Either<Set<ParseFailedException>, K> simpleParseBubble(Module module, Bubble b) {
        ParseInModule parser = simpleGetParser(module);
        Tuple2<Either<Set<ParseFailedException>, K>, Set<ParseFailedException>> res =
                parser.parseString(b.contents(), DefinitionParsing.START_SYMBOL,
                        Source.apply("generated in ResolveConfig"), -1, -1);
        if(res._1().isRight())
            return Right.apply(res._1().right().get());
        else
            return Left.apply(res._1().left().get());
    }

    public ResolveConfig(Definition def, boolean isStrict) {
        this.def = def;
        this.isStrict = isStrict;
        this.parseBubble = this::simpleParseBubble;
        this.getParser = this::simpleGetParser;
        gen = new RuleGrammarGenerator(def, isStrict);
    }

    public ResolveConfig(Definition def, boolean isStrict, BiFunction<Module, Bubble, Either<Set<ParseFailedException>, K>> parseBubble, Function<Module, ParseInModule> getParser) {
        this.def = def;
        this.isStrict = isStrict;
        this.parseBubble = parseBubble;
        this.getParser = getParser;
        gen = new RuleGrammarGenerator(def, isStrict);
    }

    public Module apply(Module inputModule) {
        if (stream(inputModule.localSentences())
                .filter(s -> s instanceof Bubble)
                .map(b -> (Bubble) b)
                .filter(b -> b.sentenceType().equals("config")).count() == 0)
            return inputModule;


        scala.collection.Set<Sentence> importedConfigurationSortsSubsortedToCell = stream(inputModule.productions())
                .filter(p -> p.att().contains("cell"))
                .map(p -> Production(Sort("Cell", ModuleName.apply("KCELLS")), Seq(NonTerminal(p.sort())))).collect(Collections.toSet());

        Module module = Module(inputModule.name(), (scala.collection.Set<Module>) inputModule.imports(),
                (scala.collection.Set<Sentence>) inputModule.localSentences().$bar(importedConfigurationSortsSubsortedToCell),
                inputModule.att());

        ParseInModule parser = getParser.apply(module);
        Set<ParseFailedException> errors = new HashSet<>();

        scala.collection.Set<Sentence> configDeclProductions = stream(module.localSentences())
                .parallel()
                .filter(s -> s instanceof Bubble)
                .map(b -> (Bubble) b)
                .filter(b -> b.sentenceType().equals("config"))
                .map(b -> parseBubble.apply(module, b))
                .flatMap(contents -> {
                    if(contents.isRight()) {
                        KApply configContents = (KApply) contents.right().get();
                        List<K> items = configContents.klist().items();
                        switch (configContents.klabel().name()) {
                        case "#ruleNoConditions":
                            return Stream.of(Configuration(items.get(0), BooleanUtils.TRUE, configContents.att()));
                        case "#ruleEnsures":
                            return Stream.of(Configuration(items.get(0), items.get(1), configContents.att()));
                        default:
                            throw KEMException.compilerError("Illegal configuration with requires clause detected.", configContents);
                        }
                    } else {
                        errors.addAll(contents.left().get());
                        return Stream.empty();
                    }
                })
                .flatMap(
                        configDecl -> stream(GenerateSentencesFromConfigDecl.gen(configDecl.body(), configDecl.ensures(), configDecl.att(), parser.getExtensionModule())))
                .collect(Collections.toSet());

        if(!errors.isEmpty())
            throw errors.iterator().next();

        Module mapModule;
        if (def.getModule("MAP").isDefined()) {
            mapModule = def.getModule("MAP").get();
        } else {
            throw KEMException.compilerError("Module MAP must be visible at the configuration declaration, in module " + module.name());
        }
        return Module(module.name(), (scala.collection.Set<Module>) module.imports().$bar(Set(mapModule)),
                (scala.collection.Set<Sentence>) module.localSentences().$bar(configDeclProductions),
                module.att());
    }
}
