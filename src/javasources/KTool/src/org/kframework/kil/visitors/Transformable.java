// Copyright (c) 2012-2014 K Team. All Rights Reserved.
package org.kframework.kil.visitors;

import org.kframework.kil.ASTNode;
import org.kframework.kil.visitors.exceptions.TransformerException;

public interface Transformable {
    public ASTNode accept(Transformer transformer) throws TransformerException;
}
