// Copyright (c) 2014-2015 K Team. All Rights Reserved.

package org.kframework.backend.java.compile;

import org.kframework.attributes.Att;
import org.kframework.attributes.Location;
import org.kframework.attributes.Source;
import org.kframework.backend.java.kil.InjectedKLabel;
import org.kframework.backend.java.kil.KCollection;
import org.kframework.backend.java.kil.KItem;
import org.kframework.backend.java.kil.KLabelConstant;
import org.kframework.backend.java.kil.KList;
import org.kframework.backend.java.kil.KSequence;
import org.kframework.backend.java.kil.Kind;
import org.kframework.backend.java.kil.Rule;
import org.kframework.backend.java.kil.Sort;
import org.kframework.backend.java.kil.Term;
import org.kframework.backend.java.kil.TermContext;
import org.kframework.backend.java.kil.Token;
import org.kframework.backend.java.kil.Variable;
import org.kframework.backend.java.symbolic.ConjunctiveFormula;
import org.kframework.backend.java.symbolic.KILtoBackendJavaKILTransformer;
import org.kframework.definition.Module;
import org.kframework.kil.Attribute;
import org.kframework.kore.K;
import org.kframework.kore.KApply;
import org.kframework.kore.KLabel;
import org.kframework.kore.compile.RewriteToTop;
import org.kframework.kore.convertors.KOREtoKIL;
import org.kframework.utils.errorsystem.KExceptionManager;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * KORE to backend KIL
 */
public class KOREtoBackendKIL extends org.kframework.kore.AbstractConstructors<org.kframework.kore.K> {

    private final TermContext context;

    public KOREtoBackendKIL(TermContext context) {
        this.context = context;
    }

    @Override
    public KLabelConstant KLabel(String name) {
        return KLabelConstant.of(name, context.definition());
    }

    @Override
    public Sort Sort(String name) {
        return Sort.of(name);
    }

    @Override
    public <KK extends org.kframework.kore.K> KList KList(List<KK> items) {
        return (KList) KCollection.upKind(
                KList.concatenate(items.stream().map(this::convertInternal).collect(Collectors.toList())),
                Kind.KLIST);
    }

    @Override
    public Token KToken(String s, org.kframework.kore.Sort sort, Att att) {
        return !sort.name().equals("KBoolean") ? Token.of(Sort(sort.name()), s) : Token.of(Sort("Bool"), s);
    }

    @Override
    public KApply KApply(KLabel klabel, org.kframework.kore.KList klist, Att att) {
        throw new AssertionError("Unsupported for now because KVariable is not a KLabel. See KApply1()");
    }

    public Term KApply1(org.kframework.kore.KLabel klabel, org.kframework.kore.KList klist, Att att) {
        return KItem.of(KLabel(klabel.name()), KList(klist.items()), context);
    }

    @Override
    public <KK extends org.kframework.kore.K> KSequence KSequence(List<KK> items, Att att) {
        KSequence.Builder builder = KSequence.builder();
        items.stream().map(this::convertInternal).forEach(builder::concatenate);
        Term kSequence = KCollection.upKind(builder.build(), Kind.K);
        return kSequence instanceof Variable ? KSequence.frame((Variable) kSequence) : (KSequence) kSequence;
    }

    @Override
    public Variable KVariable(String name, Att att) {
        return new Variable(name, Sort.of(att.<String>getOptional(Attribute.SORT_KEY).orElse("K")));
    }

    @Override
    public org.kframework.kore.KRewrite KRewrite(org.kframework.kore.K left, org.kframework.kore.K right, Att att) {
        throw new AssertionError("Should not encounter a KRewrite");
    }

    @Override
    public InjectedKLabel InjectedKLabel(org.kframework.kore.KLabel klabel, Att att) {
        return new InjectedKLabel(KLabel(klabel.name()));
    }


    public Term convert(org.kframework.kore.K k, TermContext termContext, KExceptionManager kem) {
        if (k instanceof Term)
            return (Term) k;
        return KILtoBackendJavaKILTransformer.expandAndEvaluate(termContext.global(), kem, convertInternal(k));
    }

    public Term convertInternal(org.kframework.kore.K k) {
        if (k instanceof org.kframework.kore.KToken)
            return KToken(((org.kframework.kore.KToken) k).s(), ((org.kframework.kore.KToken) k).sort(), k.att());
        else if (k instanceof org.kframework.kore.KApply)
            return KApply1(((KApply) k).klabel(), ((KApply) k).klist(), k.att());
        else if (k instanceof org.kframework.kore.KSequence)
            return KSequence(((org.kframework.kore.KSequence) k).items(), k.att());
        else if (k instanceof org.kframework.kore.KVariable)
            return KVariable(((org.kframework.kore.KVariable) k).name(), k.att());
        else if (k instanceof org.kframework.kore.InjectedKLabel)
            return InjectedKLabel(((org.kframework.kore.InjectedKLabel) k).klabel(), k.att());
        else
            throw new AssertionError("BUM!");
    }


    public Rule convert(Module module, TermContext termContext, org.kframework.definition.Rule rule, KExceptionManager kem) {
        if (rule instanceof Rule) {
            return (Rule) rule;
        }
        Rule newRule = convertInternal(module, termContext, rule);
        return KILtoBackendJavaKILTransformer.expandAndEvaluate(termContext.global(), kem, newRule);
    }

    public Rule convertInternal(Module module, TermContext termContext, org.kframework.definition.Rule rule) {
        K leftHandSide = RewriteToTop.toLeft(rule.body());
        boolean isFunction = leftHandSide instanceof KApply && module.attributesFor().apply(((KApply)leftHandSide).klabel()).contains(Attribute.FUNCTION_KEY);
        org.kframework.kil.Rule oldRule = new org.kframework.kil.Rule();
        oldRule.setAttributes(new KOREtoKIL().convertAttributes(rule.att()));
        Location loc = rule.att().getOptional(Location.class).orElse(null);
        Source source = rule.att().getOptional(Source.class).orElse(null);
        oldRule.setLocation(loc);
        oldRule.setSource(source);
        if (isFunction) {
            oldRule.putAttribute(Attribute.FUNCTION_KEY, "");
        }
        return new Rule(
                "",
                convertInternal(leftHandSide),
                convertInternal(RewriteToTop.toRight(rule.body())),
                Collections.singletonList(convertInternal(rule.requires())),
                Collections.singletonList(convertInternal(rule.ensures())),
                Collections.emptySet(),
                Collections.emptySet(),
                ConjunctiveFormula.of(termContext),
                false,
                null,
                null,
                null,
                null,
                oldRule,
                termContext,
                rule.att());
    }
}
