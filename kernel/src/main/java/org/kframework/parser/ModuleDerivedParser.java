// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.parser;

import org.kframework.attributes.Source;
import org.kframework.kore.K;
import org.kframework.kore.KORE;
import org.kframework.kore.Sort;
import org.kframework.minikore.converters.KoreToMini;
import org.kframework.parser.concrete2kore.ParseInModule;
import org.kframework.utils.errorsystem.ParseFailedException;
import scala.Tuple2;
import scala.util.Either;

import java.util.Set;

// The parser derived from the module.
public class ModuleDerivedParser implements UserParser {
    private final ParseInModule parseInModule;

    public ModuleDerivedParser(String moduleName, ParseInModule parseInModule) {
        this.parseInModule = parseInModule;
    }

    public ParseResult parse(String toParse, String fromSource, String startSymbol) {
        Sort startSymbolSort = KORE.Sort(startSymbol);
        Source source = Source.apply(fromSource);
        Tuple2<Either<Set<ParseFailedException>, K>, Set<ParseFailedException>> res =
                parseInModule.parseStringWithoutTypecheck(toParse, startSymbolSort, source);
        if (res._1().isLeft()) {
            throw res._1().left().get().iterator().next();
        }
        return new ParseResult(KoreToMini.apply(TreeNodesToKORE.down(res._1().right().get())), res._2());
    }

}
