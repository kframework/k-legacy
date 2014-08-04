package org.kframework.parser.kast;

import static org.junit.Assert.*;

import org.junit.Test;
import org.kframework.kil.Bag;
import org.kframework.kil.loader.Context;

public class KastParserTest {

	@Test
	public void test() {
		final Context context = new Context();
		System.out.println(KastParser.parse("output.txt", ".Bag", context));
		System.out.println(Bag.EMPTY);
		assertTrue(KastParser.parse("output.txt", ".Bag", context).getBody().equals(Bag.EMPTY));
	}
}
