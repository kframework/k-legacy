// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.kore.compile;

import org.kframework.Collections;
import org.kframework.builtin.Sorts;
import org.kframework.definition.Definition;
import org.kframework.definition.Module;
import org.kframework.definition.Production;
import org.kframework.definition.Sentence;
import org.kframework.definition.WithInputDefinitionModuleTransformer;
import org.kframework.kil.Attribute;
import org.kframework.kore.Sort;

import java.util.HashSet;
import java.util.Set;

import static org.kframework.Collections.*;
import static org.kframework.definition.Constructors.*;

public class GenerateSortPredicateSyntax extends WithInputDefinitionModuleTransformer {

    public GenerateSortPredicateSyntax(Definition inputDefinition) {
        super(inputDefinition);
    }

    @Override
    public Module process(Module mod, scala.collection.Set<Module> alreadyProcessedImports) {
        if (mod.name().equals("SORT-K"))
            return mod;

        Set<Sentence> res = new HashSet<>();
        for (Sort sort : iterable(mod.localSorts())) {
            Production prod = getIsSortProduction(sort);
            if (!mod.productions().contains(prod))
                res.add(prod);
        }
        scala.collection.Set<Module> newImports;
        if (mod.name().equals("BOOL-SYNTAX")) {
            newImports = alreadyProcessedImports;
            res.add(getIsSortProduction(Sorts.K()));
        } else {
            if (alreadyProcessedImports.exists(i -> i.name().equals("BOOL-SYNTAX"))) {
                newImports = alreadyProcessedImports;
            } else{
                newImports = Collections.add(apply("BOOL-SYNTAX"), alreadyProcessedImports);
            }
        }


        return Module(mod.name(), newImports, (scala.collection.Set<Sentence>) mod.localSentences().$bar(immutable(res)), mod.att());
    }

    private Production getIsSortProduction(Sort sort) {
        return Production("is" + sort.name(), Sorts.Bool(),
                        Seq(Terminal("is" + sort.name()), Terminal("("), NonTerminal(Sorts.K()), Terminal(")")),
                        Att().add(Attribute.FUNCTION_KEY).add(Attribute.PREDICATE_KEY, sort.name()));
    }
}
