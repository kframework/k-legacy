// Copyright (c) 2013-2016 K Team. All Rights Reserved.
package org.kframework.backend.java.symbolic;

import org.kframework.backend.java.builtins.BitVector;
import org.kframework.backend.java.builtins.BoolToken;
import org.kframework.backend.java.builtins.FloatToken;
import org.kframework.backend.java.builtins.IntToken;
import org.kframework.backend.java.builtins.StringToken;
import org.kframework.backend.java.builtins.UninterpretedToken;
import org.kframework.backend.java.kil.*;
import org.kframework.compile.utils.ConfigurationStructureMap;
import org.kframework.kil.ASTNode;
import org.kframework.kil.DataStructureSort;
import org.kframework.kil.ListBuiltin;
import org.kframework.kil.MapBuiltin;
import org.kframework.kil.SetBuiltin;
import org.kframework.kil.Sort;
import org.kframework.kil.loader.Context;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;


/**
 * Convert a term from the Java Rewrite engine internal representation into the KIL representation.
 *
 * @author: AndreiS
 */
public class BackendJavaKILtoKILTransformer implements Transformer {

    private final Context context;
    private final ConfigurationStructureMap configurationStructureMap;
    private final IdentityHashMap<Term, ASTNode> cache = new IdentityHashMap<>();

    public BackendJavaKILtoKILTransformer(Context context) {
        this.context = context;
        configurationStructureMap = context.getConfigurationStructureMap();
    }

    @Override
    public String getName() {
        return this.getClass().toString();
    }

    @Override
    public ASTNode transform(Hole hole) {
        //return new org.kframework.kil.FreezerHole(0);
        return org.kframework.kil.Hole.KITEM_HOLE;
    }

    @Override
    public ASTNode transform(KItem kItem) {
        if (cache.containsKey(kItem)) return cache.get(kItem);
        ASTNode kil = new org.kframework.kil.KApp(
                (org.kframework.kil.Term) kItem.kLabel().accept(this),
                (org.kframework.kil.Term) kItem.kList().accept(this));
        kil.copyAttributesFrom(kItem);
        cache.put(kItem, kil);
        return kil;
    }

    @Override
    public ASTNode transform(KItemProjection kItemProj) {
        if (cache.containsKey(kItemProj)) return cache.get(kItemProj);
        ASTNode kil = new org.kframework.kil.KItemProjection(
                Sort.of(kItemProj.kind().toString()),
                (org.kframework.kil.Term) kItemProj.term().accept(this));
        kil.copyAttributesFrom(kItemProj);
        cache.put(kItemProj, kil);
        return kil;
    }

    @Override
    public ASTNode transform(KLabelConstant kLabelConstant) {
        if (cache.containsKey(kLabelConstant)) return cache.get(kLabelConstant);
        ASTNode kil = org.kframework.kil.KLabelConstant.of(kLabelConstant.label());
        kil.copyAttributesFrom(kLabelConstant);
        cache.put(kLabelConstant, kil);
        return kil;
    }

    @Override
    public ASTNode transform(KLabelInjection kLabelInjection) {
        if (cache.containsKey(kLabelInjection)) return cache.get(kLabelInjection);
        ASTNode kil = new org.kframework.kil.KInjectedLabel(
                (org.kframework.kil.Term) kLabelInjection.term().accept(this));
        kil.copyAttributesFrom(kLabelInjection);
        cache.put(kLabelInjection, kil);
        return kil;
    }

    @Override
    public ASTNode transform(KList kList) {
        if (cache.containsKey(kList)) return cache.get(kList);
        List<org.kframework.kil.Term> terms = transformTerms(kList);
        ASTNode kil = new org.kframework.kil.KList(terms);
        kil.copyAttributesFrom(kList);
        cache.put(kList, kil);
        return kil;
    }

    @Override
    public ASTNode transform(KSequence kSequence) {
        if (cache.containsKey(kSequence)) return cache.get(kSequence);
        List<org.kframework.kil.Term> terms = transformTerms(kSequence);
        ASTNode kil = new org.kframework.kil.KSequence(terms);
        kil.copyAttributesFrom(kSequence);
        cache.put(kSequence, kil);
        return kil;
    }

    private List<org.kframework.kil.Term> transformTerms(KCollection kCollection) {
        List<org.kframework.kil.Term> terms = new ArrayList<>();
        for (Term term : kCollection) {
            terms.add((org.kframework.kil.Term) term.accept(this));
        }
        if (kCollection.hasFrame()) {
            terms.add((org.kframework.kil.Term) kCollection.frame().accept(this));
        }
        return terms;
    }

    @Override
    public ASTNode transform(BuiltinSet set) {
        if (cache.containsKey(set)) return cache.get(set);
        List<org.kframework.kil.Term> elements = new ArrayList<>();
        List<org.kframework.kil.Term> baseTerms = new ArrayList<>();
        for (Term entry : set.elements()) {
            elements.add((org.kframework.kil.Term)entry.accept(this));
        }
        Collections.sort(elements);
        for (Term term : set.baseTerms()) {
            baseTerms.add((org.kframework.kil.Term) term.accept(this));
        }
        ASTNode kil = new SetBuiltin(
                context.dataStructureSortOf(DataStructureSort.DEFAULT_SET_SORT),
                baseTerms,
                elements);
        kil.copyAttributesFrom(set);
        cache.put(set, kil);
        return kil;
    }


    @Override
    public ASTNode transform(BuiltinList builtinList) {
        if (cache.containsKey(builtinList)) return cache.get(builtinList);
        List<org.kframework.kil.Term> elementsLeft = new ArrayList<>();
        List<org.kframework.kil.Term> baseTerms = new ArrayList<>();
        List<org.kframework.kil.Term> elementsRight = new ArrayList<>();
        for (Term entry : builtinList.children) {
            elementsLeft.add((org.kframework.kil.Term)entry.accept(this));
        }
        ASTNode kil = ListBuiltin.of(context.dataStructureSortOf(DataStructureSort.DEFAULT_LIST_SORT),
                baseTerms, elementsLeft, elementsRight);
        kil.copyAttributesFrom(builtinList);
        cache.put(builtinList, kil);
        return kil;
    }

    @Override
    public ASTNode transform(BuiltinMap map) {
        if (cache.containsKey(map)) return cache.get(map);
        final Map<Term, Term> entries = map.getEntries();
        List<Term> keys = new ArrayList<>(entries.keySet());
        Collections.sort(keys);
        Map<org.kframework.kil.Term, org.kframework.kil.Term> elements = new HashMap<>();
        List<org.kframework.kil.Term> baseTerms = new ArrayList<>();
        for (Term key : keys) {
            Term value = entries.get(key);
            elements.put(
                    (org.kframework.kil.Term) key.accept(this),
                    (org.kframework.kil.Term) value.accept(this));
        }
        for (Term term : map.baseTerms()) {
            baseTerms.add((org.kframework.kil.Term) term.accept(this));
        }
        ASTNode kil = new MapBuiltin(
                context.dataStructureSortOf(DataStructureSort.DEFAULT_MAP_SORT),
                baseTerms,
                elements);
        kil.copyAttributesFrom(map);
        cache.put(map, kil);
        return kil;
    }

    @Override
    public ASTNode transform(Token token) {
        if (cache.containsKey(token)) return cache.get(token);
        ASTNode kil = org.kframework.kil.Token.kAppOf(token.sort().toFrontEnd(), token.javaBackendValue());
        kil.copyAttributesFrom(token);
        cache.put(token, kil);
        return kil;
    }

    @Override
    public ASTNode transform(Variable variable) {
        if (cache.containsKey(variable)) return cache.get(variable);
//        System.out.println("VARIABLE*************"+ variable.name()+"->"+variable.sort());
        ASTNode node = new org.kframework.kil.Variable(variable.name(), variable.sort().toFrontEnd());
//        System.out.println("NODE: "+node.toString());
//        System.out.println("**********VARIABLE"+ variable.name()+"->"+variable.sort());
        node.copyAttributesFrom(variable);
        cache.put(variable, node);
        return node;
    }

    @Override
    public ASTNode transform(InjectedKLabel injectedKLabel) {
        throw new AssertionError("should be unreachable");
    }

    @Override
    public ASTNode transform(RuleAutomatonDisjunction ruleAutomatonDisjunction) {
        throw new UnsupportedOperationException();
    }

    @Override
    public ASTNode transform(InnerRHSRewrite innerRHSRewrite) {
        throw new UnsupportedOperationException();
    }

    @Override
    public ASTNode transform(BitVector bitVector) {
        return transform((Token) bitVector);
    }

    @Override
    public ASTNode transform(BoolToken boolToken) {
        return transform((Token) boolToken);
    }

    @Override
    public ASTNode transform(Collection collection) {
        throw new UnsupportedOperationException("This method should never be called");
    }

    @Override
    public ASTNode transform(ConstrainedTerm constrainedTerm) {
        throw new UnsupportedOperationException("Not implemented, yet");
    }

    @Override
    public ASTNode transform(FloatToken floatToken) {
        return transform((Token) floatToken);
    }

    @Override
    public ASTNode transform(IntToken intToken) {
        return transform((Token) intToken);
    }

    @Override
    public ASTNode transform(KCollection kCollection) {
        throw new UnsupportedOperationException("This method should never be called");
    }

    @Override
    public ASTNode transform(KLabel kLabel) {
        throw new UnsupportedOperationException("This method should never be called");
    }

    @Override
    public ASTNode transform(MetaVariable metaVariable) {
        return transform((Token) metaVariable);
    }

    @Override
    public ASTNode transform(Rule rule) {
        throw new UnsupportedOperationException("Not implemented, yet");
    }

    @Override
    public ASTNode transform(ConjunctiveFormula conjunctiveFormula) {
        throw new UnsupportedOperationException("Not implemented, yet");
    }

    @Override
    public ASTNode transform(DisjunctiveFormula disjunctiveFormula) {
        throw new UnsupportedOperationException("Not implemented, yet");
    }

    @Override
    public ASTNode transform(StringToken stringToken) {
        return transform((Token) stringToken);
    }

    @Override
    public ASTNode transform(Term node) {
        throw new UnsupportedOperationException("This method should never be called");
    }

    @Override
    public ASTNode transform(UninterpretedToken uninterpretedToken) {
        return transform((Token) uninterpretedToken);
    }

}
