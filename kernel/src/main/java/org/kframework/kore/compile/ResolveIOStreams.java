// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.kore.compile;

import org.kframework.attributes.Location;
import org.kframework.builtin.KLabels;
import org.kframework.builtin.Sorts;
import org.kframework.definition.BasicModuleTransformer;
import org.kframework.definition.Definition;
import org.kframework.definition.Module;
import org.kframework.definition.Production;
import org.kframework.definition.Rule;
import org.kframework.definition.Sentence;
import org.kframework.definition.WithInputDefinitionModuleTransformer;
import org.kframework.kore.K;
import org.kframework.kore.KApply;
import org.kframework.kore.KLabel;
import org.kframework.kore.KList;
import org.kframework.kore.KRewrite;
import org.kframework.kore.KVariable;
import org.kframework.kore.Sort;
import org.kframework.kore.TransformK;
import org.kframework.kore.VisitK;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;
import scala.Option;
import scala.Tuple2;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Optional;
import java.util.stream.Collectors;

import static org.kframework.Collections.*;
import static org.kframework.definition.Constructors.*;
import static org.kframework.kore.KORE.*;

/**
 * Created by daejunpark on 9/6/15.
 */
public class ResolveIOStreams extends WithInputDefinitionModuleTransformer {

    private final KExceptionManager kem;

    public ResolveIOStreams(Definition definition, KExceptionManager kem) {
        super(definition);
        this.kem = kem;
    }

    /**
     * Update modules that declare stream cells in configuration,
     * by using builtin *-STREAM modules.
     * <p>
     * Steps:
     * 1. Update the init rules of the stream cells.
     * 2. Update rules that refer to 'stdin' stream.
     * 3. Import rules from *-STREAM modules (with modification of cell names).
     */
    public Module process(Module m, scala.collection.Set<Module> alreadyProcessed) {
        java.util.Set<Production> streamProductions = getStreamProductions(m);
        if (streamProductions.isEmpty()) {
            return m;
        } else {
            java.util.Set<Sentence> sentences = mutable(m.localSentences());
            // NOTE: it may seem inefficient to have duplicated for-loops here,
            //      but such a duplication makes each step much simpler;
            //      moreover the number of `streamProductions` is at most two (or possibly three later on),
            //      so that the duplication effect is not that much.
            // Step 1.
            for (Production p : streamProductions) {
                sentences = sentences.stream().map(s -> resolveInitRule(p, s)).collect(Collectors.toSet());
            }
            // Step 2.
            for (Production p : streamProductions) {
                if (p.att().<String>get("stream").get().equals("stdin")) {
                    sentences.addAll(getStdinStreamUnblockingRules(p, sentences));
                }
            }
            // Step 3.
            for (Production p : streamProductions) {
                sentences.addAll(getStreamModuleSentences(p));
            }
            return Module(m.name(), alreadyProcessed, immutable(sentences), m.att());
        }
    }

    // Collect productions that have 'stream' attribute
    private java.util.Set<Production> getStreamProductions(Module m) {
        java.util.Set<Production> productions = new HashSet<>();
        for (Sentence s : mutable(m.localSentences())) {
            if (s instanceof Production) {
                Production p = (Production) s;
                if (p.att().<String>get("stream").isDefined()) {
                    checkStreamName(p.att().<String>get("stream").get());
                    productions.add(p);
                }
            }
        }
        return productions;
    }

    private void checkStreamName(String streamName) {
        ArrayList<String> streams = new ArrayList<String>();
        streams.add("stdin");
        streams.add("stdout");

        if (!streams.contains(streamName)) {
            throw KEMException.compilerError("Make sure you give the correct stream names: " + streamName +
                    "\nIt should be one of " + streams.toString());
        }
    }

    private Sentence resolveInitRule(Production p, Sentence s) {
        if (s instanceof Rule) {
            return resolveInitRule(p, (Rule) s);
        } else {
            return s;
        }
    }

    // Step 1.
    private Rule resolveInitRule(Production streamProduction, Rule rule) {
        Sort streamSort = streamProduction.sort(); // InCell, OutCell
        String initLabel = GenerateSentencesFromConfigDecl.getInitLabel(streamSort); // initInCell, initOutCell
        KLabel cellLabel = streamProduction.klabel().get(); // <in>, <out>

        // rule initInCell(Init) => <in> ... </in>
        if (isInitRule(initLabel, cellLabel.name(), rule)) {
            KRewrite body = (KRewrite) rule.body();
            KApply right = (KApply) body.right();
            KList klist = getContentsOfInitRule(streamProduction);
            right = KApply(right.klabel(), klist, right.att());
            body = KRewrite(body.left(), right, body.att());
            return Rule(body, rule.requires(), rule.ensures(), rule.att());
        }
        return rule;
    }

    // TODO(Daejun): rule should have contained an initializer attribute.
    private boolean isInitRule(String initLabel, String cellLabel, Sentence s) {
        try {
            // rule initXCell(Init) => <x> ... </x>
            KRewrite body = (KRewrite) ((Rule) s).body();
            KApply left = (KApply) body.left();
            KApply right = (KApply) body.right();
            return left.klabel().name().equals(initLabel) // initXCell
                    && right.klabel().name().equals(cellLabel); // <x>
        } catch (ClassCastException ignored) {
            return false;
        }
    }

    // Get an initializer rule from the built-in *-STREAM module
    private KList getContentsOfInitRule(Production streamProduction) {
        String streamName = streamProduction.att().<String>get("stream").get(); // stdin, stdout
        String initLabel = GenerateSentencesFromConfigDecl.getInitLabel(
                Sort(GenerateSentencesFromConfigDecl.getSortOfCell(streamName))); // initStdinCell, initStdoutCell
        String cellLabel = "<" + streamName + ">"; // <stdin>, <stdout>

        java.util.List<Sentence> initRules =
                stream(getStreamModule(streamName).localSentences())
                        .filter(s -> isInitRule(initLabel, cellLabel, s))
                        .collect(Collectors.toList());
        assert initRules.size() == 1;
        Sentence initRule = initRules.get(0);

        // rule initXCell(Init) => <x> ... </x>
        KRewrite body = (KRewrite) ((Rule) initRule).body();
        KApply right = (KApply) body.right();
        return right.klist();
    }

    // Step 3.
    // get sentences from stream module:
    // - productions whose sort is `Stream`
    // - rules that have `stream` attribute, but changing cell names according to user-defined one.
    private java.util.Set<Sentence> getStreamModuleSentences(Production streamProduction) {
        String streamName = streamProduction.att().<String>get("stream").get(); // stdin, stdout
        String builtinCellLabel = "<" + streamName + ">"; // <stdin>, <stdout>
        KLabel userCellLabel = streamProduction.klabel().get(); // <in>, <out>

        java.util.Set<Sentence> sentences = new HashSet<>();
        for (Sentence s : mutable(getStreamModule(streamName).localSentences())) {
            if (s instanceof Rule) {
                Rule rule = (Rule) s;
                if (rule.att().contains("stream")) {
                    // Update cell names
                    K body = new TransformK() {
                        @Override
                        public K apply(KApply k) {
                            k = (KApply) super.apply(k);
                            return KApply(apply(k.klabel()), k.klist(), k.att());
                        }

                        private KLabel apply(KLabel klabel) {
                            if (klabel.name().equals(builtinCellLabel)) {
                                return userCellLabel;
                            } else {
                                return klabel;
                            }
                        }
                    }.apply(rule.body());

                    rule = Rule(body, rule.requires(), rule.ensures(), rule.att());
                    sentences.add(rule);
                }
            } else if (s instanceof Production) {
                Production production = (Production) s;
                if (production.sort().localName().equals("Stream")) { // production.sort().name().equals("Stream@K-IO")
                    sentences.add(production);
                }
            }
        }
        return sentences;
    }

    private Module getStreamModule(String streamName) {
        // TODO(Daejun): fix hard-coded stream module naming convention
        String moduleName = streamName.toUpperCase() + "-STREAM";
        Option<Module> module = inputDefinition().getModule(moduleName);
        if (module.isDefined()) {
            return module.get();
        } else {
            throw KEMException.compilerError("no such module: " + moduleName);
        }
    }

    // Step 2.
    /*
     * From the following stdin stream reference rule:
     * rule <k> read() => V ... </k>
     *      <in>
     *        ListItem(V:Int) => .List
     *        ...
     *      </in>
     *
     * Generate the following auxiliary rule:
     * rule <k> read() ... </k>
     *      <in>
     *        `.List => ListItem(#parseInput("Int", " \n\t\r"))`
     *        ListItem(#buffer(_:String))
     *        ...
     *      </in>
     */
    private java.util.Set<Sentence> getStdinStreamUnblockingRules(Production streamProduction,
                                                                  java.util.Set<Sentence> sentences) {
        KLabel userCellLabel = streamProduction.klabel().get(); // <in>

        // find rules with currently supported matching patterns
        java.util.Set<Tuple2<Rule, Sort>> rules = new HashSet<>();
        for (Sentence s : sentences) {
            if (s instanceof Rule) {
                Rule rule = (Rule) s;
                java.util.List<Sort> sorts = isSupportingRulePatternAndGetSortNameOfCast(streamProduction, rule);
                assert sorts.size() <= 1;
                if (sorts.size() == 1) {
                    rules.add(Tuple2.apply(rule, sorts.get(0)));
                }
            }
        }

        // generate additional unblocking rules for each of the above rules
        java.util.Set<Sentence> newSentences = new HashSet<>();
        for (Tuple2<Rule, Sort> r : rules) {
            Rule rule = r._1();
            Sort sort = r._2();

            K body = new TransformK() {
                @Override
                public K apply(KApply k) {
                    if (k.klabel().name().equals(userCellLabel.name())) {
                        return getUnblockRuleBody(streamProduction, sort);
                    } else {
                        return super.apply(k);
                    }
                }

                @Override
                public K apply(KRewrite k) {
                    // drop rhs
                    return apply(k.left());
                }
            }.apply(rule.body());

            rule = Rule(body, rule.requires(), rule.ensures(), rule.att());
            newSentences.add(rule);
        }

        return newSentences;
    }

    // return (a list of) sort names of cast if the given rule has the supported pattern matching over input stream cell,
    // otherwise return empty.
    // currently, the list of sort names of cast should be a singleton.
    /*
     * Currently supported rule pattern is:
     * rule <k> read() => V ... </k>
     *      <in>
     *        ListItem(V:Int) => .List
     *        ...
     *      </in>
     */
    private java.util.List<Sort> isSupportingRulePatternAndGetSortNameOfCast(Production streamProduction, Rule rule) {
        KLabel userCellLabel = streamProduction.klabel().get(); // <in>

        java.util.List<Sort> sorts = new ArrayList<>();
        new VisitK() {
            @Override
            public void apply(KApply k) {
                if (k.klabel().name().equals(userCellLabel.name())) {
                    Optional<Sort> sort = wellformedAndGetSortOfCast(k.klist());
                    if (sort.isPresent()) {
                        sorts.add(sort.get());
                    } else {
                        if (k.att().get(Location.class).isDefined()) { // warning only for user-provided rules
                            kem.registerCompilerWarning("Unsupported matching pattern in stdin stream cell." +
                                    "\nThe currently supported pattern is: <in> ListItem(V:Sort) => .List ... </in>", k);
                        }
                    }
                }
                super.apply(k);
            }

            // TODO(Daejun): it support only pattern matching on the top of stream.
            //               more patterns need to be supported as well.
            /*
             * Return cast sort name if well formed, otherwise empty string.
             *
             * klist is well formed if it consists of:
             *   #noDots(.KList)
             *   ListItem(#SemanticCastToInt(V)) => .List
             *   #dots(.KList)
             *
             * which comes from, e.g.,:
             *   <in> ListItem(V:Int) => .List ... </in>
             */
            private Optional<Sort> wellformedAndGetSortOfCast(KList klist) {
                try {
                    if (klist.size() == 3) {
                        KApply k1 = (KApply) klist.items().get(0);
                        KApply k3 = (KApply) klist.items().get(2);
                        if (k1.klabel().name().equals(KLabels.NO_DOTS) && k1.klist().size() == 0 &&
                                k3.klabel().name().equals(KLabels.DOTS) && k3.klist().size() == 0) {

                            KRewrite k2 = (KRewrite) klist.items().get(1);
                            KApply k2l = (KApply) k2.left();
                            KApply k2r = (KApply) k2.right();
                            if (k2l.klabel().name().equals("ListItem") && k2l.klist().size() == 1 &&
                                    k2r.klabel().name().equals(".List") && k2r.klist().size() == 0) {

                                KApply k2li = (KApply) k2l.klist().items().get(0);
                                if (k2li.klabel().name().startsWith("#SemanticCastTo") && k2li.klist().size() == 1 &&
                                        k2li.klist().items().get(0) instanceof KVariable) {
                                    return Optional.of(ResolveSemanticCasts.getSortOfCast(k2li));
                                }
                            }
                        }
                    }
                } catch (ClassCastException ignored) {
                }
                return Optional.empty();
            }
        }.apply(rule.body());

        return sorts;
    }

    // get rule body of the `[unblock]` rule (it should exist an unique one),
    // instantiating with proper `Sort` and `Delimiters` values.
    // this method should be called with stdin stream production, not with stdout stream.
    /*
     * Currently supporting generated rule would be:
     * rule <k> read() ... </k>
     *      <in>
     *        `.List => ListItem(#parseInput("Int", " \n\t\r"))`
     *        ListItem(#buffer(_:String))
     *        ...
     *      </in>
     */
    private K getUnblockRuleBody(Production streamProduction, Sort sort) {
        String streamName = streamProduction.att().<String>get("stream").get();
        assert streamName.equals("stdin"); // stdin
        String builtinCellLabel = "<" + streamName + ">"; // <stdin>
        KLabel userCellLabel = streamProduction.klabel().get(); // <in>

        java.util.List<Sentence> unblockRules = stream(getStreamModule(streamName).localSentences())
                .filter(s -> s instanceof Rule && s.att().contains("unblock"))
                .collect(Collectors.toList());
        assert unblockRules.size() == 1;
        Rule unblockRule = (Rule) unblockRules.get(0);

        return new TransformK() {
            @Override
            public K apply(KApply k) {
                if (k.klabel().name().startsWith("#SemanticCastToString") && k.klist().size() == 1) {
                    K i = k.klist().items().get(0);
                    if (i instanceof KVariable) {
                        KVariable x = (KVariable) i;
                        switch (x.name()) {
                        case "?Sort":
                            return KToken("\"" + sort + "\"", Sorts.String());
                        case "?Delimiters":
                            // TODO(Daejun): support `delimiter` attribute in stream cell
                            return KToken("\" \\n\\t\\r\"", Sorts.String());
                        default:
                            // fall through
                        }
                    }
                }
                k = (KApply) super.apply(k);
                return KApply(apply(k.klabel()), k.klist(), k.att());
            }

            private KLabel apply(KLabel klabel) {
                if (klabel.name().equals(builtinCellLabel)) {
                    return userCellLabel;
                } else {
                    return klabel;
                }
            }
        }.apply(unblockRule.body());
    }

}
