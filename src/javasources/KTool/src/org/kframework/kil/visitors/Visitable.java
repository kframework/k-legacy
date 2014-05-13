// Copyright (c) 2012-2014 K Team. All Rights Reserved.
package org.kframework.kil.visitors;

public interface Visitable {
    /**
     * Implements a Visitor pattern.
     * @param visitor
     */
    public void accept(Visitor visitor);
}
