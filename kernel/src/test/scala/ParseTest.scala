package org.kframework.definition

import org.kframework.attributes.Source
import org.kframework.parser.concrete2kore.ParseInModule
import org.junit.Test
import org.junit.Assert._
import org.kframework.kore.ADT.SortLookup
import org.kframework.kore.K
import org.kframework.kore.KORE._
import org.kframework.utils.errorsystem.ParseFailedException


class ParseTest {

  val expParser = new ParseInModule(test.EXP)
  val kParser = new ParseInModule(test.KDEFINITION)


  def parseTest(parser: ParseInModule, toParse: String, parseAs: SortLookup, res: K): Boolean =
    parser.parseString(toParse, parseAs, Source(""))._1 match {
      case Right(x) => x == res
      case Left(y) => false
    }

  @Test def simpExp(): Unit = {
    val res = KApply(KLabel("_+_"), KApply(KLabel("0")), KApply(KLabel("0")))
    assertTrue(parseTest(expParser, "0 + 0", test.Exp, res))
  }

  @Test def ktokens(): Unit = {
    println(kParser.parseString("\"aName0239ntehu\"", test.KString, Source("")))
    println(kParser.parseString("SortName", test.KSort, Source("")))
    println(kParser.parseString("klabel", test.KAttributeKey, Source("")))
    println(kParser.parseString("MYMODULE", test.KModuleName, Source("")))
    assertTrue(true)
  }

  @Test def kml(): Unit = {
    println(kParser.parseString("kmlvar(\"testVar\")", test.KMLVar, Source("")))
    println(kParser.parseString("KMLtrue", test.KMLFormula, Source("")))
    println(kParser.parseString("KMLfalse", test.KMLFormula, Source("")))
    println(kParser.parseString("kmlvar(\"testVar\") KMLand KMLtrue", test.KMLFormula, Source("")))
    println(kParser.parseString("kmlvar(\"testVar\") KMLor KMLfalse", test.KMLFormula, Source("")))
    println(kParser.parseString("KMLnot kmlvar(\"testVar\")", test.KMLFormula, Source("")))
    println(kParser.parseString("KMLexists kmlvar(\"testVar\") . KMLtrue", test.KMLFormula, Source("")))
    println(kParser.parseString("KMLforall kmlvar(\"testVar\") . KMLtrue", test.KMLFormula, Source("")))
    println(kParser.parseString("kmlvar(\"testVar\") KML=> KMLtrue", test.KMLFormula, Source("")))
    assertTrue(true)
  }

}
