// Copyright (c) 2014 K Team. All Rights Reserved.

package org.kframework.backend.java.kil;

import org.kframework.backend.java.symbolic.Matcher;
import org.kframework.backend.java.symbolic.Transformer;
import org.kframework.backend.java.symbolic.Unifier;
import org.kframework.backend.java.symbolic.Visitor;
import org.kframework.backend.java.util.KSorts;
import org.kframework.kil.ASTNode;

public class Tuple2<U extends Term, V extends Term> extends Term {
    public final U _1;
    public final V _2;

    public Tuple2(U _1, V _2) {
        super(Kind.KITEM);
        this._1 = _1;
        this._2 = _2;
    }

    @Override
    public boolean isSymbolic() {
        return false;
    }

    @Override
    public String sort() {
        return KSorts.KITEM;
    }

    @Override
    public boolean isExactSort() {
        return true;
    };

    @Override
    public int computeHash() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((_1 == null) ? 0 : _1.hashCode());
        result = prime * result + ((_2 == null) ? 0 : _2.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Tuple2<?, ?> other = (Tuple2<?, ?>) obj;
        if (_1 == null) {
            if (other._1 != null)
                return false;
        } else if (!_1.equals(other._1))
            return false;
        if (_2 == null) {
            if (other._2 != null)
                return false;
        } else if (!_2.equals(other._2))
            return false;
        return true;
    }

    @Override
    public ASTNode<Term> accept(Transformer transformer) {
        return transformer.transform(this);
    }

    @Override
    public void accept(Matcher matcher, Term pattern) {
        matcher.match(this, pattern);
    }

    @Override
    public void accept(Unifier unifier, Term pattern) {
        unifier.unify(this, pattern);
    }

    @Override
    public void accept(Visitor visitor) {
        visitor.visit(this);
    }

    @Override
    public Term get(int index) {
        switch (index) {
        case 0:
            return _1;
        case 1:
            return _2;
        default:
            throw new IndexOutOfBoundsException();
        }
    }

    @Override
    public int size() {
        return 2;
    }
}