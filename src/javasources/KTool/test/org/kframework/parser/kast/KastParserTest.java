// Copyright (c) 2012-2014 K Team. All Rights Reserved.
package org.kframework.parser.kast;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.kframework.kil.BoolBuiltin;
import org.kframework.kil.KList;
import org.kframework.kil.Bag;
import org.kframework.kil.Cell;
import org.kframework.kil.IntBuiltin;
import org.kframework.kil.KApp;
import org.kframework.kil.KLabelConstant;
import org.kframework.kil.StringBuiltin;
import org.kframework.kil.Term;
import org.kframework.kil.loader.Context;

public class KastParserTest {

    @Test
    public void test() {
        final Context context = new Context();
        //test input the .Bag
        assertEquals(KastParser.parse("output.txt", ".Bag", context).getBody(),Bag.EMPTY);
        //test input the number 0 and the number with ktoken format
        assertEquals(KastParser.parse("output.txt", "0", context).getBody(),IntBuiltin.of(0));
        assertEquals(KastParser.parse("output.txt", "0", context).getBody(),
                        KastParser.parse("output.txt", "`0::Int`", context).getBody());
        //test input the number 0 and the number with ktoken format without sort
        assertEquals(KastParser.parse("output.txt", "0", context).getBody(),
                        KastParser.parse("output.txt", "`0`", context).getBody());
        //test input the string "0" and the number with ktoken format
        assertEquals(KastParser.parse("output.txt", "`\"0\"`", context).getBody(),
                StringBuiltin.of("0"));
        assertEquals(KastParser.parse("output.txt", "`\"0\"`", context).getBody(),
                        KastParser.parse("output.txt", "\"0\"", context).getBody());
        assertFalse(KastParser.parse("output.txt", "`\"\"0\"\"`", context).getBody()
        		.equals(KastParser.parse("output.txt", "\"0\"", context).getBody()));
        //test input boolean format
        assertEquals(KastParser.parse("output.txt", "true", context).getBody(),
                 BoolBuiltin.of("true"));
        assertEquals(KastParser.parse("output.txt", "true", context).getBody(),
                 KastParser.parse("output.txt", "`true::Bool`", context).getBody());
        assertEquals(KastParser.parse("output.txt", "false", context).getBody(),
                BoolBuiltin.of("false"));
        assertEquals(KastParser.parse("output.txt", "false", context).getBody(),
                KastParser.parse("output.txt", "`false::Bool`", context).getBody());
        //test parsing klabel and cells
        Cell testCell = new Cell("abc",Bag.EMPTY);
        assertEquals(KastParser.parse("output.txt", "<abc> .Bag </abc>", context),
        		testCell);
        //test parsing a KItem
        assertEquals(KastParser.parse("output.txt", "`abc`(0,0)", context).getBody(),
        		KApp.of("abc", IntBuiltin.of(0), IntBuiltin.of(0)));
    }
}
