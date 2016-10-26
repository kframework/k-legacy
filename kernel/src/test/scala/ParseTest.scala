package org.kframework.definition

import org.kframework.attributes.Source
import org.kframework.parser.concrete2kore.ParseInModule
import org.junit.Test
import org.junit.Assert._

class ParseTest {

  val myParser = new ParseInModule(test.EXP)
  val parseResult = myParser.parseString("0 + 0", test.Exp, Source(""))

  @Test def main(): Unit = {
    println(parseResult._1)
    assertTrue(true)
  }


}
