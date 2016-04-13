// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.parser.concrete2kore.generator;

import org.apache.commons.collections4.trie.PatriciaTrie;
import org.kframework.Collections;
import org.kframework.attributes.Att;
import org.kframework.builtin.Sorts;
import org.kframework.compile.ConfigurationInfo;
import org.kframework.compile.ConfigurationInfoFromModule;
import org.kframework.definition.Definition;
import org.kframework.definition.DefinitionTransformer;
import org.kframework.definition.Module;
import org.kframework.definition.ModuleName;
import org.kframework.definition.ModuleTransformer;
import org.kframework.definition.Production;
import org.kframework.definition.ProductionItem;
import org.kframework.definition.RegexTerminal;
import org.kframework.definition.Sentence;
import org.kframework.definition.Terminal;
import org.kframework.definition.UserList;
import org.kframework.kil.Attribute;
import org.kframework.kil.loader.Constants;
import org.kframework.kore.Sort;
import org.kframework.kore.convertors.KOREtoKIL;
import org.kframework.parser.concrete2kore.ParseInModule;
import scala.Option;
import scala.collection.Seq;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.*;
import java.util.stream.Stream;

import static org.kframework.Collections.*;
import static org.kframework.definition.Constructors.Att;
import static org.kframework.definition.Constructors.*;
import static org.kframework.kore.KORE.*;
import static scala.compat.java8.JFunction.func;

/**
 * Generator for rule and ground parsers.
 * Takes as input a reference to a definition containing all the base syntax of K
 * and uses it to generate a grammar by connecting all users sorts in a lattice with
 * the top sort KItem#Top and the bottom sort KItem#Bottom.
 * <p>
 * The instances of the non-terminal KItem is renamed in KItem#Top if found in the right
 * hand side of a production, and into KItem#Bottom if found in the left hand side.
 */
public class RuleGrammarGenerator {

    private final Definition baseK;
    private final boolean strict;
    private static final Set<Sort> kSorts = new HashSet<>();

    static {
        kSorts.add(Sorts.KBott());
        kSorts.add(Sorts.K());
        kSorts.add(Sorts.KLabel());
        kSorts.add(Sorts.KList());
        kSorts.add(Sorts.KItem());
        kSorts.add(Sort("RuleContent", ModuleName.apply("REQUIRES-ENSURES")));
        kSorts.add(Sorts.KConfigVar());
        kSorts.add(Sorts.KString());
    }

    private static Set<Sort> kSorts() {
        return java.util.Collections.unmodifiableSet(kSorts);
    }

    /// modules that have a meaning:
    public static final String RULE_CELLS = "RULE-CELLS";
    public static final String RULE_PARSER = "RULE-PARSER";
    public static final String CONFIG_CELLS = "CONFIG-CELLS";
    public static final String K = "K";
    public static final String AUTO_CASTS = "AUTO-CASTS";
    public static final String K_TOP_SORT = "K-TOP-SORT";
    public static final String K_BOTTOM_SORT = "K-BOTTOM-SORT";
    public static final String AUTO_FOLLOW = "AUTO-FOLLOW";
    public static final String PROGRAM_LISTS = "PROGRAM-LISTS";
    public static final String RULE_LISTS = "RULE-LISTS";
    public static final String BASIC_K = "BASIC-K";
    public static final String SORT_K = "SORT-K";

    public static final String POSTFIX = "-PROGRAM-PARSING";

    public static final String ID = "ID";
    public static final String ID_PROGRAM_PARSING = ID + POSTFIX;

    /**
     * Initialize a grammar generator.
     *
     * @param baseK  A Definition containing a K module giving the syntax of K itself.
     *               The default K syntax is defined in include/kast.k.
     * @param strict true if the generated parsers should retain inferred variable
     *               sorts as sort predicate in the requires clause.
     */
    public RuleGrammarGenerator(Definition baseK, boolean strict) {
        this.baseK = DefinitionTransformer.from((Module m) -> {
            if (!m.name().equals(BASIC_K))
                return m;
            Set<Sentence> castProds = new HashSet<>();
            castProds.addAll(makeCasts(m.resolve(Sorts.KLabel()), m.resolve(Sorts.KLabel()), m.resolve(Sorts.KLabel())));
            castProds.addAll(makeCasts(m.resolve(Sorts.KList()), m.resolve(Sorts.KList()), m.resolve(Sorts.KList())));
            castProds.addAll(makeCasts(m.resolve(Sorts.KBott()), m.resolve(Sorts.K()), m.resolve(Sorts.KItem())));
            castProds.addAll(makeCasts(m.resolve(Sorts.KBott()), m.resolve(Sorts.K()), m.resolve(Sorts.K())));
            return Module(m.name(),
                    m.imports(),
                    org.kframework.Collections.addAll(m.localSentences(), immutable(castProds)),
                    m.att());
        }, "Generate cast prods for " + BASIC_K).apply(baseK);
        this.strict = strict;
    }

    private Set<Module> renameKItem2Bottom(Set<Module> def) {
        // TODO: do renaming of KItem and K in the LHS to KBott?
        return def;
    }

    /**
     * Creates the seed module that can be used to parse rules.
     * Imports module markers RULE-CELLS and K found in /include/kast.k.
     *
     * @param mod The user defined module from which to start.
     * @return a new module which imports the original user module and a set of marker modules.
     */
    public Module getRuleGrammar(Module mod) {
        // import RULE-CELLS in order to parse cells specific to rules
        Module newM = new Module(mod.name() + "-" + RULE_PARSER, Set(mod, baseK.getModule(K).get(), baseK.getModule(RULE_PARSER).get()), Set(), Att());
        return newM;
    }

    /**
     * Creates the seed module that can be used to parse configurations.
     * Imports module markers CONFIG-CELLS and K found in /include/kast.k.
     *
     * @param mod The user defined module from which to start.
     * @return a new module which imports the original user module and a set of marker modules.
     */
    public Module getConfigGrammar(Module mod) {
        // import CONFIG-CELLS in order to parse cells specific to configurations
        Module newM = new Module(mod.name() + "-" + CONFIG_CELLS, Set(mod, baseK.getModule(K).get(), baseK.getModule(CONFIG_CELLS).get()), Set(), Att());
        return newM;
    }

    /**
     * Creates the seed module that can be used to parse programs.
     * Imports module markers PROGRAM-LISTS found in /include/kast.k.
     *
     * @param mod The user defined module from which to start.
     * @return a new module which imports the original user module and a set of marker modules.
     */
    public Module getProgramsGrammar(Module mod) {
        // import PROGRAM-LISTS so user lists are modified to parse programs
        scala.collection.Set<Module> modules = Set(mod, baseK.getModule(PROGRAM_LISTS).get());

        if(!mod.name().endsWith(POSTFIX) && stream(mod.importedModules()).anyMatch(m -> m.name().equals(ID))) {
            Module idProgramParsingModule = baseK.getModule(ID_PROGRAM_PARSING).get();
            modules = add(idProgramParsingModule, modules);
        }
        return Module.apply(mod.name() + POSTFIX, modules, Set(), Att());
    }

    public static boolean isParserSort(Sort s) {
        return kSorts.contains(s) || s.name().startsWith("#");
    }

    /**
     * Create the rule parser for the given module.
     * It creates a module which includes the given module and the base K module given to the
     * constructor. The new module contains syntax declaration for Casts and the diamond
     * which connects the user concrete syntax with K syntax.
     *
     * @param seedMod module for which to create the parser.
     * @return parser which applies disambiguation filters by default.
     */
    public ParseInModule getCombinedGrammar(Module seedMod) {
        /** Extension module is used by the compiler to get information about subsorts and access the definition of casts */
        Module extensionM = ModuleTransformer.from((Module m) -> {
            Set<Sentence> newProds = new HashSet<>();
            if (baseK.getModule(AUTO_CASTS).isDefined() && seedMod.importedModules().exists(func(m1 -> m1.name().equals(AUTO_CASTS)))) { // create the casts
                for (Sort srt : iterable(m.localSorts())) {
                    if (!isParserSort(srt)) {
                        // K ::= K "::Sort" | K ":Sort" | K "<:Sort" | K ":>Sort"
                        newProds.addAll(makeCasts(Sorts.KBott(), Sorts.K(), srt));
                    }
                }
            }
            if (baseK.getModule(K_TOP_SORT).isDefined() && seedMod.importedModules().exists(func(m1 -> m1.name().equals(K_TOP_SORT)))) { // create the upper diamond
                for (Sort srt : iterable(m.localSorts())) {
                    if (!isParserSort(srt)) {
                        // K ::= Sort
                        newProds.add(Production(Sorts.K(), Seq(NonTerminal(srt)), Att().add(Att.generatedByAutomaticSubsorting())));
                    }
                }
            }
            if (baseK.getModule(K_BOTTOM_SORT).isDefined() && seedMod.importedModules().exists(func(m1 -> m1.name().equals(K_BOTTOM_SORT)))) { // create the lower diamond
                for (Sort srt : iterable(m.localSorts())) {
                    if (!isParserSort(srt)) {
                        // Sort ::= KBott
                        newProds.add(Production(srt, Seq(NonTerminal(Sorts.KBott())), Att().add(Att.generatedByAutomaticSubsorting())));
                    }
                }
            }
            if (newProds.isEmpty())
                return m;
            return Module(m.name(),
                    org.kframework.Collections.add(baseK.getModule(K).get(), m.imports()),
                    org.kframework.Collections.addAll(m.localSentences(), immutable(newProds)),
                    m.att());
        }, "Generate Extension module").apply(seedMod);

        // prepare for auto follow restrictions, which needs to be done globally (if necessary)
        Object PRESENT = new Object();
        PatriciaTrie<Object> terminals = new PatriciaTrie<>(); // collect all terminals so we can do automatic follow restriction for prefix terminals
        mutable(extensionM.productions()).stream().forEach(p -> stream(p.items()).forEach(i -> {
            if (i instanceof Terminal) terminals.put(((Terminal) i).value(), PRESENT);
        }));

        /** Disambiguation module is used by the parser to have an easier way of disambiguating parse trees. */
        Module disambM = ModuleTransformer.from((Module m) -> {
            // make sure a configuration actually exists, otherwise ConfigurationInfoFromModule explodes.
            final ConfigurationInfo cfgInfo = m.localSentences().exists(func(p -> p instanceof Production && p.att().contains("cell")))
                    ? new ConfigurationInfoFromModule(extensionM)
                    : null;

            if (baseK.getModule(RULE_CELLS).isDefined() && seedMod.importedModules().exists(func(m1 -> m1.name().equals(RULE_CELLS))) &&
                    cfgInfo != null) { // prepare cell productions for rule parsing
                Set<Sentence> newProds = stream(m.localSentences()).flatMap(s -> {
                    if (s instanceof Production && (s.att().contains("cell"))) {
                        Production p = (Production) s;
                        // assuming that productions tagged with 'cell' start and end with terminals, and only have non-terminals in the middle
                        assert p.items().head() instanceof Terminal || p.items().head() instanceof RegexTerminal;
                        assert p.items().last() instanceof Terminal || p.items().last() instanceof RegexTerminal;
                        final ProductionItem body;
                        if (cfgInfo.isLeafCell(p.sort())) {
                            body = p.items().tail().head();
                        } else {
                            body = NonTerminal(Sort("Bag"));
                        }
                        final ProductionItem optDots = NonTerminal(Sort("#OptionalDots"));
                        Seq<ProductionItem> pi = Seq(p.items().head(), optDots, body, optDots, p.items().last());
                        Production p1 = Production(p.klabel().get().name(), Sort("Cell", ModuleName.apply("KCELLS")), pi, p.att());
                        Production p2 = Production(Sort("Cell", ModuleName.apply("KCELLS")), Seq(NonTerminal(p.sort())));
                        return Stream.of(p1, p2);
                    }
                    if (s instanceof Production && (s.att().contains("cellFragment"))) {
                        Production p = (Production) s;
                        Production p1 = Production(Sort("Cell", ModuleName.apply("KCELLS")), Seq(NonTerminal(p.sort())));
                        return Stream.of(p, p1);
                    }
                    return Stream.of(s);
                }).collect(Collectors.toSet());
                m = Module(m.name(), org.kframework.Collections.add(baseK.getModule(RULE_CELLS).get(), m.imports()), immutable(newProds), m.att());
            }

            // configurations can be declared on multiple modules, so make sure to subsort previously declared cells to Cell
            if (baseK.getModule(CONFIG_CELLS).isDefined() && seedMod.importedModules().exists(func(m1 -> m1.name().equals(CONFIG_CELLS)))) {
                Set<Sentence> newProds = stream(m.localSentences()).flatMap(s -> {
                    if (s instanceof Production && s.att().contains("initializer")) {
                        Production p = (Production) s;
                        // assuming that productions tagged with 'cell' start and end with terminals, and only have non-terminals in the middle
                        assert p.items().head() instanceof Terminal || p.items().head() instanceof RegexTerminal;
                        final ProductionItem body;
                        Production p1 = Production(Sort("Cell", ModuleName.apply("KCELLS")), Seq(NonTerminal(p.sort())));
                        return Stream.of(s, p1);
                    }
                    return Stream.of(s);
                }).collect(Collectors.toSet());
                m = Module(m.name(), org.kframework.Collections.add(baseK.getModule(CONFIG_CELLS).get(), m.imports()), immutable(newProds), m.att());
            }

            if (baseK.getModule(AUTO_FOLLOW).isDefined() && seedMod.importedModules().exists(func(m1 -> m1.name().equals(AUTO_FOLLOW)))) {
                Set<Sentence> newProds = stream(m.localSentences()).map(s -> {
                    if (s instanceof Production) {
                        Production p = (Production) s;
                        if (p.sort().name().startsWith("#"))
                            return p; // don't do anything for such productions since they are advanced features
                        // rewrite productions to contain follow restrictions for prefix terminals
                        // example _==_ and _==K_ can produce ambiguities. Rewrite the first into _(==(?![K])_
                        // this also takes care of casting and productions that have ":"
                        Seq<ProductionItem> items = map(pi -> {
                            if (pi instanceof Terminal) {
                                Terminal t = (Terminal) pi;
                                if (t.value().trim().equals(""))
                                    return pi;
                                Set<String> follow = new HashSet<>();
                                terminals.prefixMap(t.value()).keySet().stream().filter(biggerString -> !t.value().equals(biggerString))
                                        .forEach(biggerString -> {
                                            String ending = biggerString.substring(t.value().length());
                                            follow.add(ending);
                                        });
                                // add follow restrictions for the characters that might produce ambiguities
                                if (!follow.isEmpty()) {
                                    return Terminal.apply(t.value(), follow.stream().collect(toList()));
                                }
                            }
                            return pi;
                        }, p.items());
                        if (p.klabel().isDefined())
                            p = Production(p.klabel().get().name(), p.sort(), Seq(items), p.att());
                        else
                            p = Production(p.sort(), Seq(items), p.att());
                        return p;
                    }
                    return s;
                }).collect(Collectors.toSet());
                m = Module(m.name(), m.imports(), immutable(newProds), m.att());
            }

            if (baseK.getModule(RULE_LISTS).isDefined() && seedMod.importedModules().exists(func(m1 -> m1.name().equals(RULE_LISTS)))) {
                Set<Sentence> newProds = mutable(m.localSentences());
                for (UserList ul : UserList.getLists(newProds)) {
                    org.kframework.definition.Production prod1;
                    // Es ::= E
                    prod1 = Production(ul.sort, Seq(NonTerminal(ul.childSort)));
                    newProds.add(prod1);
                }
                m = Module(m.name(), m.imports(), immutable(newProds), m.att());
            }
            return m;
        }, "Generate Disambiguation module").apply(extensionM);

        /** Parsing module is used to generate the grammar for the kernel of the parser. */
        Module parseM = ModuleTransformer.from((Module m) -> {
            if (baseK.getModule(PROGRAM_LISTS).isDefined() && seedMod.importedModules().exists(func(m1 -> m1.name().equals(PROGRAM_LISTS)))) {
                Set<Sentence> newProds = mutable(m.localSentences());
                // if no start symbol has been defined in the configuration, then use K
                for (Sort srt : iterable(m.localSorts())) {
                    if (!kSorts.contains(srt) && !m.listSorts().contains(srt)) {
                        // K ::= Sort
                        newProds.add(Production(baseK.getModule(SORT_K).get().resolve(Sorts.K()), Seq(NonTerminal(srt)), Att()));
                    }
                }
                java.util.List<UserList> uLists = UserList.getLists(newProds);
                // eliminate the general list productions
                newProds = newProds.stream().filter(p -> !(p instanceof Production && p.att().contains(Att.userList()))).collect(Collectors.toSet());
                // for each triple, generate a new pattern which works better for parsing lists in programs.
                for (UserList ul : uLists) {
                    Production prod1, prod2, prod3, prod4, prod5;

                    Att newAtts = ul.attrs.remove("userList");
                    // Es#Terminator ::= "" [klabel('.Es)]
                    prod1 = Production(ul.terminatorKLabel, Sort(ul.sort.localName() + "#Terminator"), Seq(Terminal("")),
                            newAtts.add("klabel", ul.terminatorKLabel).add(Constants.ORIGINAL_PRD, ul.pTerminator));
                    // Ne#Es ::= E "," Ne#Es [klabel('_,_)]
                    prod2 = Production(ul.klabel, Sort("Ne#" + ul.sort.localName()),
                            Seq(NonTerminal(ul.childSort), Terminal(ul.separator), NonTerminal(Sort("Ne#" + ul.sort.localName()))),
                            newAtts.add("klabel", ul.klabel).add(Constants.ORIGINAL_PRD, ul.pList));
                    // Ne#Es ::= E Es#Terminator [klabel('_,_)]
                    prod3 = Production(ul.klabel, Sort("Ne#" + ul.sort.localName()),
                            Seq(NonTerminal(ul.childSort), NonTerminal(Sort(ul.sort.localName() + "#Terminator"))),
                            newAtts.add("klabel", ul.klabel).add(Constants.ORIGINAL_PRD, ul.pList));
                    // Es ::= Ne#Es
                    prod4 = Production(ul.sort, Seq(NonTerminal(Sort("Ne#" + ul.sort.localName()))));
                    // Es ::= Es#Terminator // if the list is *
                    prod5 = Production(ul.sort, Seq(NonTerminal(Sort(ul.sort.localName() + "#Terminator"))));

                    newProds.add(prod1);
                    newProds.add(prod2);
                    newProds.add(prod3);
                    newProds.add(prod4);
                    newProds.add(SyntaxSort(Sort(ul.sort.localName() + "#Terminator")));
                    newProds.add(SyntaxSort(Sort("Ne#" + ul.sort.localName())));
                    if (!ul.nonEmpty) {
                        newProds.add(prod5);
                    }
                }
                return Module(m.name(), Collections.add(baseK.getModule(SORT_K).get(), m.imports()), immutable(newProds), m.att());
            }
            return m;
        }, "Generate Parsing module").apply(disambM);

        return new ParseInModule(seedMod, extensionM, disambM, parseM, this.strict);
    }

    private Set<Sentence> makeCasts(Sort outerSort, Sort innerSort, Sort castSort) {
        Set<Sentence> prods = new HashSet<>();
        Att attrs1 = Att().add(Att.sort(), castSort);
        // annotations with :Sort@MODULE
        prods.add(Production("#SyntacticCast", castSort, Seq(NonTerminal(castSort), Terminal("::" + castSort.name())), attrs1));
        prods.add(Production("#SemanticCastTo" + castSort.name(), castSort, Seq(NonTerminal(castSort), Terminal(":" + castSort.name())), attrs1));
        prods.add(Production("#InnerCast", outerSort, Seq(NonTerminal(castSort), Terminal("<:" + castSort.name())), attrs1));
        prods.add(Production("#OuterCast", castSort, Seq(NonTerminal(innerSort), Terminal(":>" + castSort.name())), attrs1));
        // annotations with :Sort
        prods.add(Production("#SyntacticCast", castSort, Seq(NonTerminal(castSort), Terminal("::" + castSort.localName())), attrs1));
        prods.add(Production("#SemanticCastTo" + castSort.name(), castSort, Seq(NonTerminal(castSort), Terminal(":" + castSort.localName())), attrs1));
        prods.add(Production("#InnerCast", outerSort, Seq(NonTerminal(castSort), Terminal("<:" + castSort.localName())), attrs1));
        prods.add(Production("#OuterCast", castSort, Seq(NonTerminal(innerSort), Terminal(":>" + castSort.localName())), attrs1));
        return prods;
    }
}
