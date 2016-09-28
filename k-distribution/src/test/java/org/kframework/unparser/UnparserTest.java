package org.kframework.unparser;


import org.junit.Before;
import org.junit.Test;
import org.kframework.Kapi;
import org.kframework.attributes.Source;
import org.kframework.definition.Module;
import org.kframework.definition.NonTerminal;
import org.kframework.definition.Production;
import org.kframework.kompile.CompiledDefinition;
import org.kframework.kore.K;
import org.kframework.kore.KApply;
import org.kframework.kore.KToken;
import scala.Option;
import scala.collection.Set;

import java.util.function.BiFunction;
import java.util.stream.Collectors;

public class UnparserTest {


    private Module module;

    @Test
    void simple() {
        K k = parse("foo()");
        unparse(k);
    }

    String unparse(K k) {
        if (k instanceof KApply) {
            KApply kapply = (KApply) k;
            Production production = module.productionsFor().get(kapply.klabel()).get().toStream().find(p -> p.items().count(item -> item instanceof NonTerminal) == kapply.klist().size()).get();
            String format = production.att().<String>getOptional("format").get();
            String children = kapply.klist().stream().map(this::unparse).collect(Collectors.joining());
            return children;
        } else if (k instanceof KToken) {
            return ((KToken) k).s();
        } else {
            throw new AssertionError("Unimplemented still");
        }
    }

    @Before
    private void setupTest() {
        String definitionString = "the definition text";
        Kapi kapi = new Kapi();
        CompiledDefinition impDefinition = kapi.kompile(definitionString, "IMP");
        module = impDefinition.programParsingModuleFor(impDefinition.mainSyntaxModuleName(), kapi.kapiGlobal.kem).get();
        parser = impDefinition.getParser(module, impDefinition.programStartSymbol, kapi.kapiGlobal.kem);
    }

    private BiFunction<String, Source, K> parser;

    public K parse(String s) {
        return parser.apply(s, Source.apply("genereted in unit test"));
    }
}
