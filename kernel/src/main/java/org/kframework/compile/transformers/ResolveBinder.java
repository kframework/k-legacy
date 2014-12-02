// Copyright (c) 2012-2014 K Team. All Rights Reserved.
package org.kframework.compile.transformers;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;

import org.kframework.compile.utils.SyntaxByTag;
import org.kframework.kil.ASTNode;
import org.kframework.kil.KLabelConstant;
import org.kframework.kil.Module;
import org.kframework.kil.ModuleItem;
import org.kframework.kil.Production;
import org.kframework.kil.Sort;
import org.kframework.kil.loader.Context;
import org.kframework.kil.visitors.CopyOnWriteTransformer;
import org.kframework.utils.errorsystem.KExceptionManager;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ResolveBinder extends CopyOnWriteTransformer {

    private static final KLabelConstant BINDER_PREDICATE
            = KLabelConstant.of(AddPredicates.predicate(Sort.of("Binder")));
    private static final KLabelConstant BOUNDED_PREDICATE
            = KLabelConstant.of(AddPredicates.predicate(Sort.of("Bound")));
    private static final KLabelConstant BOUNDING_PREDICATE
            = KLabelConstant.of(AddPredicates.predicate(Sort.of("Bounding")));

    private static final String REGEX
            = "\\s*(\\d+)(\\s*-\\>\\s*(\\d+))?\\s*(,?)";

    public ResolveBinder(Context context) {
        super("Resolve binder", context);
    }

    @Override
    public ASTNode visit(Module node, Void _)  {
        Set<Production> prods = SyntaxByTag.get(node, "binder", context);
        prods.addAll(SyntaxByTag.get(node, "metabinder", context));
        if (prods.isEmpty())
            return node;

        List<ModuleItem> items = new ArrayList<ModuleItem>(node.getItems());
        node = node.shallowCopy();
        node.setItems(items);

        for (Production prod : prods) {
            String bindInfo = prod.getAttribute("binder");
            if (bindInfo == null || bindInfo.equals("")) {
                bindInfo = prod.getAttribute("metabinder");
                if (bindInfo == null || bindInfo.equals("")) {
                    bindInfo = "1->" + prod.getArity();
                }
            }
            Pattern p = Pattern.compile(REGEX);
            Matcher m = p.matcher(bindInfo);
            Multimap<Integer, Integer> bndMap = HashMultimap.create();

            while (m.regionStart() < m.regionEnd()) {
                if (!m.lookingAt()) {
                    throw KExceptionManager.criticalError(
                            "could not parse binder attribute \"" + bindInfo.substring(m.regionStart(), m.regionEnd()) + "\"");
                }
                if (m.end() < m.regionEnd()) {
                    if (!m.group(4).equals(",")) {
                        throw KExceptionManager.criticalError("expecting ',' at the end \"" + m.group() + "\"");
                    }
                } else {
                    if (!m.group(4).equals("")) {
                        throw KExceptionManager.criticalError("unexpected ',' at the end \"" + m.group() + "\"");
                    }
                }

                int bndIdx = Integer.parseInt(m.group(1)) - 1; //rebasing  bindings to start at 0
                if (m.group(3) == null) {
                    for (int idx = 0; idx < prod.getArity(); idx++) {
                        if (idx != bndIdx)
                            bndMap.put(bndIdx, idx);
                    }
                } else {
                    bndMap.put(bndIdx, Integer.parseInt(m.group(3)) - 1);  //rebasing positions to start at 0
                }

                m.region(m.end(), m.regionEnd());
            }

            prod.setBinderMap(bndMap);
        }

        return node;
    }
}
