package org.kframework.backend.kore.latex;

import org.kframework.definition.*;
import org.kframework.kore.compile.VisitKORE;
import org.kframework.kore.*;

/**
 * Created by Edgar Pek on 9/28/15.
 */
public class LatexKOREVisitor extends VisitKORE {
    final private String prefixDebug = "\t[DEBUG] KORE visitor ";

    public void  visitSentence(Sentence s) {
        s.att().apply();
    }


    @Override
    public Void apply(KApply k) {
        System.out.println(prefixDebug + "KApply " + k.toString());
        //k.klist().items().stream().forEach(this::apply);
        return null;
    }

    @Override
    public Void apply(KRewrite k) {
        System.out.println(prefixDebug + "KRewrite " + k.toString());
        //apply(k.left());
        //apply(k.right());
        return null;
    }

    @Override
    public Void apply(KToken k) {
        System.out.println(prefixDebug + "KToken " + k.toString());
        return null;
    }

    @Override
    public Void apply(KVariable k) {
        System.out.println(prefixDebug + "KVariable " + k.toString());
        return null;
    }

    @Override
    public Void apply(KSequence k) {
        System.out.println(prefixDebug + "KSequence " + k.toString());
        //k.items().stream().forEach(this::apply);
        return null;
    }

    @Override
    public Void apply(InjectedKLabel k) {
        System.out.println(prefixDebug + "InjectedLabel " + k.toString());
        return null;
    }

}
