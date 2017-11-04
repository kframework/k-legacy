package org.kframework.definition

import org.kframework.attributes.Source
import org.kframework.parser.concrete2kore.ParseInModule
import org.junit.Test
import org.junit.Assert._

class ParseTest {

  val expParser = new ParseInModule(test.EXP)
  val kParser = new ParseInModule(test.KDEFINITION)

  @Test def simpExp(): Unit = {
    println(kParser.parseString("0 + 0", test.Exp, Source("")))
    assertTrue(true)
  }

  @Test def ktokens(): Unit = {
    println(kParser.parseString("\"aName0239ntehu\"", test.KString, Source("")))
    println(kParser.parseString("SortName", test.KSort, Source("")))
    println(kParser.parseString("klabel", test.KAttributeKey, Source("")))
    println(kParser.parseString("MYMODULE", test.KModuleName, Source("")))
    assertTrue(true)
  }

  @Test def kml(): Unit = {
    println(kParser.parseString("kmlvar(testVar)", test.KMLVar, Source("")))
    println(kParser.parseString("KMLtrue", test.KMLFormula, Source("")))
    println(kParser.parseString("KMLfalse", test.KMLFormula, Source("")))
    println(kParser.parseString("kmlvar(testVar) KMLand KMLtrue", test.KMLFormula, Source("")))
    println(kParser.parseString("kmlvar(testVar) KMLor KMLfalse", test.KMLFormula, Source("")))
    println(kParser.parseString("KMLnot kmlvar(testVar)", test.KMLFormula, Source("")))
    println(kParser.parseString("KMLexists kmlvar(testVar) . KMLtrue", test.KMLFormula, Source("")))
    println(kParser.parseString("KMLforall kmlvar(testVar) . KMLtrue", test.KMLFormula, Source("")))
    println(kParser.parseString("kmlvar(testVar) KML=> KMLtrue", test.KMLFormula, Source("")))
    assertTrue(true)
  }

}
