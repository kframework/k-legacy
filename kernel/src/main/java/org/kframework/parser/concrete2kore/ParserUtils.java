// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.parser.concrete2kore;

import org.apache.commons.io.FileUtils;
import org.kframework.attributes.Att;
import org.kframework.attributes.Source;
import org.kframework.definition.Module;
import org.kframework.kil.Definition;
import org.kframework.kil.DefinitionItem;
import org.kframework.kil.Require;
import org.kframework.kil.loader.Context;
import org.kframework.kompile.Kompile;
import org.kframework.kore.K;
import org.kframework.kore.Sort;
import org.kframework.kore.convertors.KILtoKORE;
import org.kframework.main.GlobalOptions;
import org.kframework.parser.concrete2kore.generator.RuleGrammarGenerator;
import org.kframework.parser.outer.Outer;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;

import java.io.File;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.kframework.Collections.*;
import static org.kframework.definition.Constructors.*;

/**
 * A few functions that are a common pattern when calling the new parser.
 */
public class ParserUtils {

    private final KExceptionManager kem;
    private final GlobalOptions options;
    private Function<File, File> makeAbsolute;

    public ParserUtils(Function<File, File> makeAbsolute, KExceptionManager kem) {
        this(makeAbsolute, kem, new GlobalOptions());
    }

    public ParserUtils(Function<File, File> makeAbsolute, KExceptionManager kem, GlobalOptions options) {
        this.makeAbsolute = makeAbsolute;
        this.kem = kem;
        this.options = options;
    }

    public static K parseWithFile(String theTextToParse,
                                  String mainModule,
                                  Sort startSymbol,
                                  File definitionFile) {
        String definitionText;
        try {
            definitionText = FileUtils.readFileToString(definitionFile);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return parseWithString(theTextToParse, mainModule, startSymbol, Source.apply(definitionFile.getAbsolutePath()), definitionText);
    }

    public static K parseWithString(String theTextToParse,
                                    String mainModule,
                                    Sort startSymbol,
                                    Source source,
                                    String definitionText) {
        Module kastModule = parseMainModuleOuterSyntax(definitionText, source, mainModule);
        return parseWithModule(theTextToParse, startSymbol, source, kastModule);
    }

    public static K parseWithModule(String theTextToParse,
                                    Sort startSymbol,
                                    Source source,
                                    org.kframework.definition.Module kastModule) {
        ParseInModule parser = new ParseInModule(kastModule);
        return parseWithModule(theTextToParse, startSymbol, source, parser);
    }

    public static K parseWithModule(String theTextToParse,
                                    Sort startSymbol,
                                    Source source,
                                    ParseInModule kastModule) {
        return kastModule.parseString(theTextToParse, startSymbol, source)._1().right().get();
    }

    /**
     * Takes a definition in e-kore textual format and a main module name, and returns the KORE
     * representation of that module. Current implementation uses JavaCC and goes through KIL.
     *
     * @param definitionText textual representation of the modules.
     * @param mainModule     main module name.
     * @return KORE representation of the main module.
     */
    public static Module parseMainModuleOuterSyntax(String definitionText, Source source, String mainModule) {
        Definition def = new Definition();
        def.setItems(Outer.parse(source, definitionText, null));
        def.setMainModule(mainModule);
        def.setMainSyntaxModule(mainModule);

        Context context = new Context();
        new CollectProductionsVisitor(context).visitNode(def);

        KILtoKORE kilToKore = new KILtoKORE(context);
        return kilToKore.apply(def).getModule(mainModule).get();
    }

    public List<org.kframework.kil.Module> slurp(
            String definitionText,
            Source source,
            List<File> lookupDirectories) {
        return slurp(definitionText, source, lookupDirectories, new ArrayDeque<>());
    }

    private List<org.kframework.kil.Module> slurp(
            String definitionText,
            Source source,
            List<File> lookupDirectories,
            Deque<File> parents) {
        List<DefinitionItem> items = Outer.parse(source, definitionText, null);
        if (options.verbose) {
            System.out.println("Importing: " + source);
        }
        List<org.kframework.kil.Module> results = new ArrayList<>();

        for (DefinitionItem di : items) {
            if (di instanceof org.kframework.kil.Module) {
                results.add((org.kframework.kil.Module) di);
            } else if (di instanceof Require) {
                // resolve location of the new file

                String definitionFileName = ((Require) di).getValue();

                Optional<File> definitionFile = lookupDirectories.stream()
                        .map(lookupDirectory -> {
                            if (new File(definitionFileName).isAbsolute()) {
                                return new File(definitionFileName);
                            } else {
                                return new File(lookupDirectory, definitionFileName);
                            }
                        })
                        .filter(file -> file.exists()).findFirst();

                ArrayList<File> allLookupDirectories = new ArrayList<>(lookupDirectories);
                allLookupDirectories.add(0, definitionFile.get().getParentFile());

                if (definitionFile.isPresent()) {
                    // Look for dependency cycle
                    if (parents.stream().
                            anyMatch(parent -> {
                                try {
                                    File sourceFile = new File(source.source());
                                    return parent.getCanonicalFile().equals(sourceFile.getCanonicalFile());
                                } catch (IOException e) {
                                    // Catch exceptions from getCanonicalFile
                                    return false;
                                }
                            })) {
                        String dependencyChain = (new File(source.source())).getName() + " -> "
                                + parents.stream().map(File::getName).collect(Collectors.joining(" -> "));
                        throw KExceptionManager.criticalError("Dependency cycle detected: " + dependencyChain, di);
                    } else {
                        parents.push(new File(source.source()));
                    }

                    results.addAll(slurp(loadDefinitionText(definitionFile.get()),
                            Source.apply(definitionFile.get().getAbsolutePath()),
                            allLookupDirectories,
                            parents));
                }
                else
                    throw KExceptionManager.criticalError("Could not find file: " +
                            definitionFileName + "\nLookup directories:" + allLookupDirectories, di);
            }
        }

        if (!parents.isEmpty()) {
            File p = parents.pop();
        }

        return results;
    }

    private String loadDefinitionText(File definitionFile) {
        try {
            return FileUtils.readFileToString(makeAbsolute.apply(definitionFile));
        } catch (IOException e) {
            throw KEMException.criticalError(e.getMessage(), e);
        }
    }

    public Set<Module> loadModules(
            Set<Module> previousModules,
            String definitionText,
            Source source,
            List<File> lookupDirectories,
            boolean autoImportDomains) {

        List<org.kframework.kil.Module> kilModules =
                slurp(definitionText, source, lookupDirectories);

        Definition def = new Definition();
        def.setItems((List<DefinitionItem>) (Object) kilModules);

        Context context = new Context();
        new CollectProductionsVisitor(context).visitNode(def);

        KILtoKORE kilToKore = new KILtoKORE(context, false, autoImportDomains);

        HashMap<String, Module> koreModules = new HashMap<>();
        koreModules.putAll(previousModules.stream().collect(Collectors.toMap(Module::name, m -> m, (a, b) -> a)));
        HashSet<org.kframework.kil.Module> kilModulesSet = new HashSet<>(kilModules);

        kilModules.stream().forEach(m -> kilToKore.apply(m, kilModulesSet, koreModules));

        // TODO(dwightguth): from radum: why are you removing the previous modules? Just spent 30 minutes debugging because of this
        // please add javadoc to this function, since I don't know why you will need such a behaviour
        return koreModules.values().stream().filter(m -> !previousModules.contains(m)).collect(Collectors.toSet());
    }

    public org.kframework.definition.Definition loadDefinition(
            String mainModuleName,
            String syntaxModuleName,
            String definitionText,
            File source,
            File currentDirectory,
            List<File> lookupDirectories,
            boolean autoImportDomains) {
        ArrayList<File> allLookupDirectories = new ArrayList<>();
        allLookupDirectories.addAll(lookupDirectories);
        allLookupDirectories.add(currentDirectory);
        return loadDefinition(mainModuleName, syntaxModuleName, definitionText,
                Source.apply(source.getAbsolutePath()),
                allLookupDirectories, autoImportDomains);
    }

    public org.kframework.definition.Definition loadDefinition(
            String mainModuleName,
            String syntaxModuleName,
            String definitionText,
            Source source,
            List<File> lookupDirectories,
            boolean autoImportDomains) {
        Set<Module> previousModules = new HashSet<>();
        if (autoImportDomains)
            previousModules.addAll(loadModules(new HashSet<>(), Kompile.REQUIRE_PRELUDE_K, source, lookupDirectories, false));
        Set<Module> modules = loadModules(previousModules, definitionText, source, lookupDirectories, autoImportDomains);
        modules.addAll(previousModules); // add the previous modules, since load modules is not additive
        Optional<Module> opt = modules.stream().filter(m -> m.name().equals(mainModuleName)).findFirst();
        if (!opt.isPresent()) {
            throw KEMException.compilerError("Could not find main module with name " + mainModuleName
                    + " in definition. Use --main-module to specify one.");
        }
        Module mainModule = opt.get();
        opt = modules.stream().filter(m -> m.name().equals(syntaxModuleName)).findFirst();
        Module syntaxModule;
        if (!opt.isPresent()) {
            kem.registerCompilerWarning("Could not find main syntax module with name " + syntaxModuleName
                    + " in definition.  Use --syntax-module to specify one. Using " + mainModuleName + " as default.");
            syntaxModule = mainModule;
        } else {
            syntaxModule = opt.get();
        }

        return RuleGrammarGenerator.autoGenerateBaseKCasts(org.kframework.definition.Definition.apply(mainModule, immutable(modules), Att().add(Att.syntaxModule(), syntaxModule.name())));
    }
}
