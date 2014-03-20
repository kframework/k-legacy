package org.kframework.compile.transformers;

import org.kframework.compile.utils.MaudeHelper;
import org.kframework.compile.utils.MetaK;
import org.kframework.kil.*;
import org.kframework.kil.Collection;
import org.kframework.kil.loader.Context;
import org.kframework.kil.visitors.CopyOnWriteTransformer;
import org.kframework.kil.visitors.exceptions.TransformerException;
import org.kframework.krun.K;
import org.kframework.utils.general.GlobalSettings;

import java.util.*;
import java.util.List;
import java.util.Set;

/**
 * Transforms syntax declarations for concrete user sorts into KLabel declaration.
 */

public class FlattenSyntax extends CopyOnWriteTransformer {
    Set<String> listSeparators = new HashSet<String>();
    boolean isComputation = false;

    public FlattenSyntax(Context context) {
        super("Syntax K to Abstract K", context);
    }

    @Override
    public ASTNode transform(Module node) throws TransformerException {
        listSeparators.clear();
        node = (Module) super.transform(node);
        if (listSeparators.isEmpty())
            return node;

        for (String sep : listSeparators) {
            node.addConstant(KSorts.KLABEL, MetaK.getListUnitLabel(sep));
        }
        return node;
    }

    @Override
    public ASTNode transform(Syntax node) throws TransformerException {
        if (!MetaK.isComputationSort(node.getSort().getName())) {
            isComputation = false;
            return super.transform(node);
        }
        isComputation = true;
        node = (Syntax) super.transform(node);
        node.setSort(new Sort(KSorts.KLABEL));
        return node;
    }

    @Override
    public ASTNode transform(Production node) throws TransformerException {
        if (node.containsAttribute("KLabelWrapper"))
            return node;
        if (!isComputation)
            return super.transform(node);
        if (node.isSubsort() && !node.containsAttribute("klabel"))
            return null;
        String arity = String.valueOf(node.getArity());
        Attributes attrs = node.getAttributes().shallowCopy();
        if (node.isListDecl()) {
            listSeparators.add(((UserList) node.getItems().get(0)).getSeparator());
            attrs.set("hybrid", "");
        }
        node = node.shallowCopy();
        List<ProductionItem> pis = new ArrayList<ProductionItem>();
        pis.add(new Terminal(node.getKLabel()));
        node.setItems(pis);
        attrs.set("arity", arity);
        node.setAttributes(attrs);
        node.setSort(KSorts.KLABEL);
        return node;
    }

    @Override
    public ASTNode transform(Sort node) throws TransformerException {
        if (!MetaK.isComputationSort(node.getName()))
            return node;
        return new Sort("K");
    }

}
