// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework;

import org.junit.Test;
import org.kframework.attributes.Att;
import org.kframework.definition.Definition;
import org.kframework.definition.Module;
import org.kframework.utils.errorsystem.KEMException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import static org.kframework.Collections.*;
import static org.kframework.frontend.KORE.*;
import static org.kframework.definition.Constructors.*;

public class DefinitionParserTest {
    @Test
    public void testEmpty() throws Exception {
        Definition actual = DefinitionParser.from("require \"domains.k\" module X endmodule");
        Module expected = Module("X", Set());
        //assertEquals(Definition(expected, Set(expected), Att()), actual);
    }

    @Test
    public void testOnlySyntax() throws Exception {
        org.kframework.definition.Definition actual = DefinitionParser.from(
                "require \"domains.k\"" + "\n" +
                "module X" + "\n" +
                "syntax Foo ::= \"bar\"" + "\n" +
                "endmodule");
        Module expected = Module("X", Set(
                Production(Sort("Foo"), Seq(Terminal("bar")), Att().add(Att.klabel(), "bar"))
        ));
        //assertEquals(Definition.apply(expected, Set(expected), Att()), actual);
    }

    @Test
    public void testWithRuleBody() throws Exception {
        org.kframework.definition.Definition actual = DefinitionParser.from(
                "require \"domains.k\"" + "\n" +
                "module X" + "\n" +
                "syntax Foo ::= \"bar\"" + "\n" +
                "rule bar => bar" + "\n" +
                "endmodule");
        Module expected = Module("X", Set(
                Production(Sort("Foo"), Seq(Terminal("bar")), Att().add(Att.klabel(), "bar"))
        ));
        //assertEquals(Definition.apply(expected, Set(expected), Att()), actual);
    }

    @Test
    public void testNonexistantRequires() throws Exception {
        try {
            DefinitionParser.from(
                    "require \"nonexistant-file.k\"" + "\n" +
                            "module X" + "\n" +
                            "syntax Foo ::= \"bar\"" + "\n" +
                            "rule bar => bar" + "\n" +
                            "endmodule");
            fail("Parser not failing to find 'nonexistant-file.k'.");
        } catch (KEMException e) {
            assertEquals(e.getMessage().split("\\n")[0], "[Error] Critical: Could not find file: nonexistant-file.k");
        }
    }
}
