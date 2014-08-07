package org.kframework.parser.kast;

import static org.junit.Assert.*;

import org.junit.Test;
import org.kframework.kil.Bag;
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
		System.out.println(KastParser.parse("output.txt", "<abc> .Bag </abc>", context));
		System.out.println(KastParser.parse("output.txt", "`abc`(0,0)", context));
	}
}
