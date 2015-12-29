package org.kframework.kompile;

import org.kframework.definition.Definition;
import org.kframework.definition.Module;
import org.kframework.kil.DefinitionItem;

import java.io.Serializable;
import java.util.LinkedList;
import java.util.TreeSet;

/**
 * Created by Edgar Pek on 9/16/15.
 * - Stores KIL outer syntax and concrete syntax of the top-level Modules
 *  (i.e. modules that are not part of K's standard library)
 */
public class ConcreteDefinition implements Serializable{
    private final LinkedList<DefinitionItem> outerKILSyntax;
    private final Definition concreteDefinition;
    public ConcreteDefinition(LinkedList<DefinitionItem> outerKILSyntax, Definition concreteDefinition) {
        this.outerKILSyntax  = outerKILSyntax;
        this.concreteDefinition = concreteDefinition;
    }

    public LinkedList<DefinitionItem> getOuterKILSyntax()     { return this.outerKILSyntax; }
    public Definition                 getConcreteDefinition() { return this.concreteDefinition; }

}
