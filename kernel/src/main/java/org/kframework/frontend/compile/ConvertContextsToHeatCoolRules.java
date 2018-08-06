// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.frontend.compile;

import org.kframework.Collections;
import org.kframework.definition.Definition;
import org.kframework.builtin.BooleanUtils;
import org.kframework.builtin.Sorts;
import org.kframework.definition.Context;
import org.kframework.definition.Module;
import org.kframework.definition.Production;
import org.kframework.definition.ProductionItem;
import org.kframework.definition.Sentence;
import org.kframework.kompile.KompileOptions;
import org.kframework.frontend.FindK;
import org.kframework.frontend.KORE;
import org.kframework.frontend.Sort;
import org.kframework.frontend.VisitK;
import org.kframework.frontend.K;
import org.kframework.frontend.KApply;
import org.kframework.frontend.KLabel;
import org.kframework.frontend.KRewrite;
import org.kframework.frontend.KVariable;
import org.kframework.utils.errorsystem.KEMException;
import scala.Option;
import scala.Tuple2;
import scala.collection.Seq;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.kframework.Collections.*;
import static org.kframework.definition.Constructors.Att;
import static org.kframework.definition.Constructors.*;
import static org.kframework.frontend.KORE.*;

public class ConvertContextsToHeatCoolRules {

    private final KompileOptions kompileOptions;

    public ConvertContextsToHeatCoolRules(KompileOptions kompileOptions) {
        this.kompileOptions = kompileOptions;
    }

    public Definition resolve(Definition d) {
        klabels = new HashSet<>();
        Module transformedMainModule = resolve(d.mainModule());
        return Definition.apply(transformedMainModule, add(transformedMainModule, minus(d.mainModule(), d.entryModules())), d.att());
    }

    public Module resolve(Module input) {
        Set<Sentence> rulesToAdd = stream(input.sentences())
                .filter(s -> s instanceof Context)
                .map(s -> (Context) s)
                .flatMap(c -> this.resolve(c, input)).collect(Collectors.toCollection(HashSet::new));
        if (!rulesToAdd.isEmpty()) {
            rulesToAdd.add(SyntaxSort(Sort("K")));
        }
        return Module(input.name(), input.imports(), (scala.collection.Set<Sentence>) stream(input.localSentences()).filter(s -> !(s instanceof Context)).collect(Collections.toSet()).$bar(immutable(rulesToAdd)), input.att());
    }

    private Set<KLabel> klabels;

    private Tuple2<KLabel, Tuple2<Seq<Sort>, Sort>> getUniqueFreezerLabel(Module input, String name, Integer pos) {
        String nameHint = pos == null ? name : name + pos;
        if (klabels.isEmpty()) {
            klabels.addAll(mutable(input.definedKLabels()));
        }
        int counter = 0;
        KLabel freezer;
        do {
            freezer = KLabel("#freezer" + nameHint + (counter++ == 0 ? "" : counter));
        } while (klabels.contains(freezer));
        klabels.add(freezer);

        Tuple2<Seq<Sort>, Sort> sig;
        if (kompileOptions.minikore) {
            Option<scala.collection.Set<Tuple2<Seq<Sort>, Sort>>> sigSetOpt = input.signatureFor().get(KLabel(name));
            if (sigSetOpt.isEmpty()) assert false;
            scala.collection.Set<Tuple2<Seq<Sort>, Sort>> sigSet = sigSetOpt.get();
            if (sigSet.size() != 1) assert false;
            sig = sigSet.iterator().next();
        } else {
            sig = null;
        }

        return Tuple2.apply(freezer, sig);
    }

    private Stream<? extends Sentence> resolve(Context context, Module input) {
        checkContextValidity(context);
        final SortedMap<KVariable, K> vars = new TreeMap<>((v1, v2) -> v1.name().compareTo(v2.name()));
        K body = context.body();
        K requiresHeat = context.requires();
        K requiresCool = BooleanUtils.TRUE;

        int currentHolePosition[] = new int[] { 0 };
        int finalHolePosition[] = new int[] { 0 };
        int currentPosition[] = new int[] { 0 };
        int firstHolePosition[] = new int[] { -1 };
        final ArrayList<Integer> nonHolePositions = new ArrayList<>();
        // Find a heated hole
        // e.g., context ++(HOLE => lvalue(HOLE))
        K heated = new VisitK() {
            K heated;
            KVariable holeVar;

            public K process(K k) {
                apply(k);
                if (heated != null)
                    return heated;
                else
                    return holeVar;
            }

            @Override
            public void apply(KRewrite k) {
                heated = k.right();
                super.apply(k);
            }

            @Override
            public void apply(KVariable k) {
                if (!k.name().equals("HOLE")) {
                    vars.put(k, k);
                    finalHolePosition[0] = currentHolePosition[0];
                    nonHolePositions.add(currentPosition[0]);
                } else {
                    holeVar = k;
                    currentHolePosition[0]++;
                    if (firstHolePosition[0] == -1) firstHolePosition[0] = currentPosition[0];
                }
                currentPosition[0]++;
                super.apply(k);
            }

            @Override
            public void apply(KApply k) {
                if (k.klabel() instanceof KVariable)
                    vars.put((KVariable) k.klabel(), InjectedKLabel(k.klabel()));
                super.apply(k);
            }
        }.process(body);

        K cooled = RewriteToTop.toLeft(body);
        // TODO(dwightguth): generate freezers better for pretty-printing purposes
        List<ProductionItem> items = new ArrayList<>();
        Tuple2<KLabel, Tuple2<Seq<Sort>, Sort>> freezerLabelAndSig;
        int pos = kompileOptions.minikore ? firstHolePosition[0] : finalHolePosition[0];
        if (cooled instanceof KApply) {
            freezerLabelAndSig = getUniqueFreezerLabel(input, ((KApply)cooled).klabel().name(), pos);
        } else {
            freezerLabelAndSig = getUniqueFreezerLabel(input, "", null);
        }
        KLabel freezerLabel = freezerLabelAndSig._1();
        Tuple2<Seq<Sort>, Sort> freezerLabelSig = freezerLabelAndSig._2();
        items.add(Terminal(freezerLabel.name()));
        items.add(Terminal("("));
        if (kompileOptions.minikore) {
            for (int i : nonHolePositions) {
                items.add(NonTerminal(freezerLabelSig._1().apply(i)));
                items.add(Terminal(","));
            }
            assert (nonHolePositions.size() == vars.size());
        } else {
            for (int i = 0; i < vars.size(); i++) {
                items.add(NonTerminal(Sort("K")));
                items.add(Terminal(","));
            }
        }
        if (vars.size() > 0) {
            items.remove(items.size() - 1);
        }
        items.add(Terminal(")"));
        Sort targetSort = kompileOptions.minikore ? freezerLabelSig._2() : Sorts.KItem();
        Production freezer = Production(freezerLabel.name(), targetSort, immutable(items), Att());
        K frozen = KApply(freezerLabel, vars.values().stream().collect(Collections.toList()));
        return Stream.of(freezer,
                Rule(KRewrite(cooled, KSequence(heated, frozen)), requiresHeat, BooleanUtils.TRUE, context.att().add("heat")),
                Rule(KRewrite(KSequence(heated, frozen), cooled), requiresCool, BooleanUtils.TRUE, context.att().add("cool")));
    }

    /**
     * Check validity of context.
     * <p>
     * Currently the following conditions are checked:
     * - Contexts must have at least one HOLE.
     * - Contexts must have a single rewrite.
     * - Only the HOLE can be rewritten in a context definition.
     *
     * @param context to be checked
     */
    public static void checkContextValidity(Context context) {
        K body = context.body();

        int cntHoles = new FindK() {
            @Override
            public scala.collection.Set<K> apply(KVariable k) {
                if (k.name().equals("HOLE")) {
                    return org.kframework.Collections.Set(k);
                } else {
                    return super.apply(k);
                }
            }
        }.apply(body).size();
        if (cntHoles < 1) {
            throw KEMException.compilerError("Contexts must have at least one HOLE.", context);
        }

        int cntRewrites = new FindK() {
            @Override
            public scala.collection.Set<K> apply(KRewrite k) {
                return this.merge(org.kframework.Collections.Set(k), super.apply(k));
            }
        }.apply(body).size();
        if (cntRewrites > 1) {
            throw KEMException.compilerError("Cannot compile a context with multiple rewrites.", context);
        }

        new VisitK() {
            @Override
            public void apply(KRewrite k) {
                if (!isHOLE(k.left())) {
                    throw KEMException.compilerError("Only the HOLE can be rewritten in a context definition", context);
                }
                super.apply(k);
            }

            // return true when k is either HOLE or #SemanticCastToX(HOLE)
            private boolean isHOLE(K k) {
                if (k instanceof KApply) {
                    KApply kapp = (KApply) k;
                    return kapp.klabel().name().startsWith("#SemanticCastTo") &&
                            kapp.klist().size() == 1 &&
                            isHOLEVar(kapp.klist().items().get(0));
                } else {
                    return isHOLEVar(k);
                }
            }

            private boolean isHOLEVar(K k) {
                return k instanceof KVariable && ((KVariable) k).name().equals("HOLE");
            }
        }.apply(body);
    }
}
