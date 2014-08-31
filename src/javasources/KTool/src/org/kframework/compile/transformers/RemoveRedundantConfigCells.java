// Copyright (c) 2014 K Team. All Rights Reserved.
package org.kframework.compile.transformers;

import org.kframework.kil.*;
import org.kframework.kil.loader.Constants;
import org.kframework.kil.loader.Context;
import org.kframework.kil.visitors.CopyOnWriteTransformer;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

import java.util.List;
import java.util.Set;

/**
 * Removes redundant configuration cells to reduce the depth of terms and, thus,
 * to speed up the Java rewrite engine.
 *
 * @author YilongL
 *
 */
public class RemoveRedundantConfigCells extends CopyOnWriteTransformer {

    private Set<String> cells = Sets.newHashSet();

    private enum Status { RULE, CONFIGURATION };

    private Status status;

    private static Set<String> TRIVIAL_CELL_ATTR = ImmutableSet.of(Constants.COLOR_color_ATTR);

    public RemoveRedundantConfigCells(Context context) {
        super("Remove redundant configuration cells", context);
    }

    @Override
    public ASTNode visit(Module node, Void _) {
        int configIdx = -1;
        Configuration config = null;
        for (int i = 0; i < node.getItems().size(); i++) {
            ModuleItem modItem = node.getItems().get(i);
            if (modItem instanceof Rule) {
                this.visitNode(modItem);
            } else if (modItem instanceof Configuration) {
                assert config == null;
                configIdx = i;
                config = (Configuration) modItem;
            }
        }

        Configuration newConfig = (Configuration) this.visitNode(config);
//        System.out.println(newConfig);
        node = node.shallowCopy();
        List<ModuleItem> items = Lists.newArrayList(node.getItems());
        // TODO(YilongL): how to preserve the original configuration information?
        items.set(configIdx, newConfig);
        node.setItems(items);
        return node;
    }

    @Override
    public ASTNode visit(Rule node, Void _) {
        status = Status.RULE;
        return super.visit(node, _);
    }

    @Override
    public ASTNode visit(Cell node, Void _) {
        if (status == Status.RULE) {
            cells.add(node.getLabel());
            return super.visit(node, _);
        } else if (status == Status.CONFIGURATION) {
            Term content = node.getContents();

            if (!(content instanceof Cell || content instanceof Bag && !((Bag) content).isEmpty())) {
                // do not remove leaf node
                return node;
            }

            Term transformedContent = (Term) super.visitNode(content);
            if (transformedContent != content && transformedContent instanceof Bag) {
                transformedContent = flattenBag((Bag) transformedContent);
            }

            if (!cells.contains(node.getLabel())
                    && Sets.difference(node.getCellAttributes().keySet(), TRIVIAL_CELL_ATTR).isEmpty()) {
                /* the current cell is not mentioned in the rule and it contains no special cell attribute */
                return transformedContent;
            }

            if (transformedContent != content) {
                node = node.shallowCopy();
                node.setContents(transformedContent);
            }
            return node;
        } else {
            assert false : "dead code";
            return null;
        }
    }

    private Term flattenBag(Bag bag) {
        if (bag.getContents().size() == 1) {
            return bag.getContents().get(0);
        }

        Bag newBag = bag.shallowCopy();
        List<Term> contents = Lists.newArrayList();
        for (Term term : bag.getContents()) {
            if (term instanceof Bag) {
                contents.addAll(((Bag) term).getContents());
            } else {
                contents.add(term);
            }
        }
        newBag.setContents(contents);

        return newBag;
    }

    @Override
    public ASTNode visit(Configuration node, Void _) {
        status = Status.CONFIGURATION;
        return super.visit(node, _);
    }

    @Override
    public ASTNode visit(org.kframework.kil.Context node, Void _) {
        return node;
    }

    @Override
    public ASTNode visit(Syntax node, Void _) {
        return node;
    }

}
