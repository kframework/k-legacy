// Copyright (c) 2014-2016 K Team. All Rights Reserved.
package org.kframework.parser.concrete2kore.disambiguation;

import com.google.common.collect.Sets;
import org.kframework.attributes.Att;
import org.kframework.parser.Ambiguity;
import org.kframework.parser.KList;
import org.kframework.parser.SetsTransformerWithErrors;
import org.kframework.parser.Term;
import org.kframework.parser.TermCons;
import org.kframework.utils.errorsystem.KException;
import org.kframework.utils.errorsystem.ParseFailedException;
import scala.util.Either;
import scala.util.Left;
import scala.util.Right;

import java.util.Set;


/**
 * Remove parsing artifacts such as single element ambiguities.
 */
public class TreeCleanerVisitor extends SetsTransformerWithErrors<ParseFailedException> {
    @Override
    public Either<Set<ParseFailedException>, Term> apply(TermCons tc) {
        if (tc.production().isSyntacticSubsort()) {
            // eliminating syntactic subsort
            Either<java.util.Set<ParseFailedException>, Term> rez = new TreeCleanerVisitor2(tc).apply(tc.get(0));
            if (rez.isLeft())
                return rez;
            if (tc.production().klabel().isEmpty())
                return apply(rez.right().get());
        } else {
            int a = 1 + 2;
        }
        if (!tc.production().att().contains("bracket") && tc.production().klabel().isEmpty()) {
            return Left.apply(Sets.newHashSet(new ParseFailedException(new KException(
                    KException.ExceptionType.ERROR, KException.KExceptionGroup.INNER_PARSER,
                    "Only subsort productions are allowed to have no #klabel attribute", tc.source().get(), tc.location().get()))));
        }
        return super.apply(tc);
    }

    /**
     * Remove KList artifacts from parsing only when it contains a single element.
     */
    @Override
    public Either<Set<ParseFailedException>, Term> apply(KList node) {
        Either<Set<ParseFailedException>, Term> res = super.apply(node);

        if (res.isRight() && ((KList) res.right().get()).items().size() == 1)
            return Right.apply(((KList) res.right().get()).items().get(0));
        else
            return res;
    }

    /**
     * Remove duplicate chain productions from the AST like K ::= Exp -> Exp ::= Int [klabel(exp2int)] -> Int ::= KBott.
     * Some of productions are excepted though, and are annotated with 'allowChainSubsort'.
     */
    private static class TreeCleanerVisitor2 extends SetsTransformerWithErrors<ParseFailedException> {
        private final TermCons parent;

        public TreeCleanerVisitor2(TermCons parent) {
            this.parent = parent;
        }

        public Either<java.util.Set<ParseFailedException>, Term> apply(Ambiguity tc) {
            return mapChildrenWithClone(tc)._1();
        }

        public Either<java.util.Set<ParseFailedException>, Term> apply(TermCons tc) {
            if (tc.production().isSyntacticSubsort()
                    && !tc.production().att().contains(Att.allowChainSubsort())
                    && !parent.production().att().contains(Att.allowChainSubsort())
                    && (tc.production().att().contains(Att.generatedByAutomaticSubsorting()) || parent.production().att().contains(Att.generatedByAutomaticSubsorting()))) {
                String msg = "Unexpected error, please report! Generated subsorts are not allowed as a child of a user subsort." +
                        "\nParent: " + parent.production().toString() +
                        "\nChild:  " + tc.production().toString();
                return Left.apply(Sets.newHashSet(new ParseFailedException(new KException(
                        KException.ExceptionType.ERROR, KException.KExceptionGroup.INTERNAL,
                        msg, tc.source().get(), tc.location().get()))));
            }
            return Right.apply(tc);
        }
    }
}
