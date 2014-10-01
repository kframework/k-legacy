// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.compile.transformers;

import org.kframework.kil.*;
import org.kframework.kil.loader.CollectProductionsVisitor;
import org.kframework.kil.loader.Context;
import org.kframework.kil.visitors.CopyOnWriteTransformer;

import java.util.*;
import java.util.Collection;

/**
 * Completes the sort lattice as follows:
 * 1. adds a bottom sort #Bot;
 * 2. adds a syntactic production List{#Bot, separator}, for each syntactic list separator;
 * 3. subsorts Sort1 to Sort2 if all subsorts of Sort1 are subsorts of Sort2 and Sort1 only
 *    contains subsorting productions;
 * 4. adds List{Sort2, separator} if List{Sort1, separator} and Sort2 < Sort1
 * 5. subsorts List{Sort1, separator} to List{Sort2, separator} if Sort1 is subsorted to Sort2
 * 6. subsorts List{#Bot, separator} to List{Sort, separator}; and
 * 7. subsorts List{Sort, separator} to KResult if Sort is subsorted to KResult
 *
 * Does NOT support syntactic lists of syntactic lists.
 * Supports at most ONE syntactic list for each sort and separator.
 *
 * The fields of the associated {@link Context} instance may not be updated.
 *
 * @author AndreiS
 */
public class CompleteSortLattice extends CopyOnWriteTransformer {

    private final Map<Sort, Set<Sort>> sortEqClasses = new HashMap<>();

    public CompleteSortLattice(Context context) {
        super("Subsort syntactic lists", context);
    }

    @Override
    public ASTNode visit(Module node, Void _)  {
        Module transformedNode = node.shallowCopy();
        transformedNode.setItems(new ArrayList<>(node.getItems()));

        Set<String> separators = new HashSet<>();
        for (Production production : context.listProductions.values()) {
            UserList userList = (UserList) production.getItems().get(0);
            separators.add(userList.getSeparator());
        }

        /* Add bottom sort #Bot */
        transformedNode.addProduction(
                Sort.BUILTIN_BOT,
                new Production(
                        new NonTerminal(Sort.BUILTIN_BOT),
                        Collections.<ProductionItem>emptyList()));

        /* Add list of bottom for each syntactic list separator (i.e. List{#Bot, separator}) */
        for (String separator : separators) {
            transformedNode.addProduction(
                    Sort.BUILTIN_BOT.getUserListSort(separator),
                    new UserList(Sort.BUILTIN_BOT, separator));
        }

        /*
         * Subsorts Sort1 to Sort2 if all productions of Sort1 are subsorting of sorts which are
         * also subsorts of Sort2 (i.e. Sort1 < Sort2 if syntax Sort1 ::= Sort3 and Sort3 < Sort2
         * for each production of Sort1).
         */
        boolean change;
        do {
            change = false;

        sort1_loop:
            for (Sort sort1 : node.getAllSorts()) {
                Collection<Production> sort1Prods = node.getProductionsOf(sort1);
                if (sort1Prods.isEmpty()) {
                    continue;
                }

                for (Production production : sort1Prods) {
                    if (!production.isSubsort()) {
                        continue sort1_loop;
                    }
                }

            sort2_loop:
                for (Sort sort2 : node.getAllSorts()) {
                    if (sort1.equals(sort2)
                            || context.isSubsorted(sort2, sort1)) {
                        continue;
                    }

                    for (Production production : sort1Prods) {
                        if (!context.isSubsorted(sort2, production.getChildSort(0))) {
                            continue sort2_loop;
                        }
                    }

                    if (context.isSubsorted(sort1, sort2)) {
                        // `sort1 < sort2` and `sort2 < sort1`, make them equal
                        Set<Sort> sort1EqCls = sortEqClasses.get(sort1);
                        Set<Sort> sort2EqCls = sortEqClasses.get(sort2);
                        if (sort1EqCls != null && sort2EqCls != null) {
                            for (Sort s : sort2EqCls) {
                                sortEqClasses.put(s, sort1EqCls);
                                sort1EqCls.add(s);
                            }
                        } else if (sort1EqCls != null) {
                            sort1EqCls.add(sort2);
                            sortEqClasses.put(sort2, sort1EqCls);
                        } else if (sort2EqCls != null) {
                            sort2EqCls.add(sort1);
                            sortEqClasses.put(sort1, sort2EqCls);
                        } else {
                            Set<Sort> eqCls = new HashSet<>();
                            eqCls.add(sort1);
                            eqCls.add(sort2);
                            sortEqClasses.put(sort1, eqCls);
                            sortEqClasses.put(sort2, eqCls);
                        }
                        // no new subsorts added, no need to loop
                        change = false;
                    } else {
                        transformedNode.addSubsort(sort2, sort1, context);
                        change = true;
                    }
                }
            }

            // TODO(AndreiS): subsort queries should implement lazy subsort computation
            context.computeSubsortTransitiveClosure();
        } while (change);

        context.setEquivalentSorts(sortEqClasses);

        /*
         * Add list of Sort2 for a separator if there is some syntactic list of Sort1 with the
         * same separator such that Sort2 is a subsort of Sort1
         * (i.e. List{Sort2, separator} if List{Sort1, separator} and Sort2 < Sort1)
         */
        for (Production production1 : context.listProductions.values()) {
            UserList userList1 = (UserList) production1.getItems().get(0);

            Set<Sort> subsorts = new HashSet<>();
            for (Production production2 : context.listProductions.values()) {
                UserList userList2 = (UserList) production2.getItems().get(0);

                if (userList1.getSeparator().equals(userList2.getSeparator())) {
                    if (context.isSubsorted(userList1.getSort(), userList2.getSort())) {
                        subsorts.add(userList2.getSort());
                    }
                }
            }

            for (Sort sort : node.getAllSorts()) {
                if (!subsorts.contains(sort) && context.isSubsorted(userList1.getSort(), sort)) {
                    transformedNode.addProduction(
                            sort.getUserListSort(userList1.getSeparator()),
                            new UserList(sort, userList1.getSeparator()));
                }
            }
        }
        /* update syntactic lists conses information in the context */
        new CollectProductionsVisitor(context).visitNode(transformedNode);

        /*
         * Subsort one syntactic list to another syntactic list with the same separator if the
         * sort of the elements of the first list is subsorted to the sort of the elements of the
         * second list (i.e. List{Sort1, separator} < List{Sort2, separator} if Sort1 < Sort2)
         */
        for (Production production1 : context.listProductions.values()) {
            UserList userList1 = (UserList) production1.getItems().get(0);

            for (Production production2 : context.listProductions.values()) {
                UserList userList2 = (UserList) production2.getItems().get(0);

                if (userList1.getSeparator().equals(userList2.getSeparator())) {
                    if (context.isSubsorted(userList1.getSort(), userList2.getSort())) {
                        transformedNode.addSubsort(
                                production1.getSort(),
                                production2.getSort(),
                                context);
                    }
                }
            }
        }

        /*
         * Subsort list of bottom to each syntactic list with the same separator
         * (i.e. List{#Bot, separator} < List{Sort, separator})
         */
        for (Production production : context.listProductions.values()) {
            UserList userList = (UserList) production.getItems().get(0);

            if (userList.getSort().equals(Sort.BUILTIN_BOT)) {
                continue;
            }

            transformedNode.addSubsort(
                    production.getSort(),
                    Sort.BUILTIN_BOT.getUserListSort(userList.getSeparator()),
                    context);
        }

        /*
         * Subsort a syntactic list to KResult if the sort of the elements of the list is
         * subsorted to KResult (i.e. List{Sort, separator} < KResult if Sort < KResult)
         */
        for (Production production : context.listProductions.values()) {
            UserList userList = (UserList) production.getItems().get(0);

            if (context.isSubsorted(Sort.KRESULT, userList.getSort())) {
                transformedNode.addSubsort(Sort.KRESULT, production.getSort(), context);
            }
        }

        if (transformedNode.getItems().size() != node.getItems().size()) {
            return transformedNode;
        } else {
            return node;
        }
    }

}
