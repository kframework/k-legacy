// Copyright (c) 2012-2014 K Team. All Rights Reserved.
package org.kframework.parser.kast;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.kframework.kil.KList;
import org.kframework.kil.Bag;
import org.kframework.kil.Cell;
import org.kframework.kil.IntBuiltin;
import org.kframework.kil.KApp;
import org.kframework.kil.KLabelConstant;
import org.kframework.kil.Term;
import org.kframework.kil.loader.Context;

public class KastParserTest {

    @Test
    public void test() {
        final Context context = new Context();
        //test input the .Bag
        assertTrue(KastParser.parse("output.txt", ".Bag", context).getBody().equals(Bag.EMPTY));
        
        //test input the number 0 and the number with ktoken format
        assertTrue(KastParser.parse("output.txt", "0", context).getBody()
                        .equals(KastParser.parse("output.txt", "`0::Int`", context).getBody()));
        
        //test input the number 0 and the number with ktoken format without sort
        assertTrue(KastParser.parse("output.txt", "0", context).getBody()
                .equals(KastParser.parse("output.txt", "`0`", context).getBody()));
        
        //test input the string "0" and the number with ktoken format
        assertTrue(KastParser.parse("output.txt", "`\"0\"`", context).getBody()
                .equals(KastParser.parse("output.txt", "\"0\"", context).getBody()));
        
        //test input boolean format
        assertTrue(KastParser.parse("output.txt", "true", context).getBody()
                .equals(KastParser.parse("output.txt", "`true::Bool`", context).getBody()));
        assertTrue(KastParser.parse("output.txt", "false", context).getBody()
                .equals(KastParser.parse("output.txt", "`false::Bool`", context).getBody()));
        
        //test parsing klabel and cells
        Cell testCell = new Cell("abc",Bag.EMPTY);
        assertTrue(KastParser.parse("output.txt", "<abc> .Bag </abc>", context)
                    .getBody().equals(testCell));
        
        //test parsing a KItem
        List<Term> testKList = new ArrayList<Term>();
        testKList.add(IntBuiltin.of(0));
        testKList.add(IntBuiltin.of(0));
        assertTrue(KastParser.parse("output.txt", "`abc`(0,0)", context)
                .getBody().equals(new KApp(KLabelConstant.of("abc"), (Term)(new KList(testKList)))));
    }
}
