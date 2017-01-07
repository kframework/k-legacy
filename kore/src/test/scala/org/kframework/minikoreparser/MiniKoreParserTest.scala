package org.kframework.minikoreparser

import org.junit.Test
import org.kframework.minikore.MiniKore._
import org.kframework.parser.minikore.MiniKoreParser;
import scala.collection._

class MiniKoreParserTest {
  @Test def emptyDefinitionParse: Unit = {
    val emptyDef: String = "[] "
    val parsedEmptyDefinition: Definition = MiniKoreParser.parse(emptyDef)
    assert(parsedEmptyDefinition.equals(Definition(LinearSeq.empty, LinearSeq.empty)))
  }

  @Test def emptyModuleParse: Unit = {
    val emptyDef: String = "[] module A endmodule []"
    val parsedEmptyDefinition: Definition = MiniKoreParser.parse(emptyDef)
    assert(parsedEmptyDefinition.equals(Definition(LinearSeq.apply(
      Module("A", LinearSeq.empty, LinearSeq.empty)), LinearSeq.empty)))
  }

  @Test def simpleDefinitionTest: Unit = {
    val simpleDef: String = "[] module A imports B [] endmodule []"
    val parsedSimpleDefinition: Definition = MiniKoreParser.parse(simpleDef)
    assert(parsedSimpleDefinition.equals(Definition(LinearSeq(Module
    ("A", LinearSeq.apply(Import("B", LinearSeq.empty)), LinearSeq.empty)), LinearSeq.empty)))
  }

  @Test def simpleAttributesTest: Unit = {
    val simpleDef: String = "[\\true()] module A imports B [] endmodule []"
    val parsedSimpleDefinition: Definition = MiniKoreParser.parse(simpleDef)
    assert(parsedSimpleDefinition.equals(Definition(LinearSeq.apply(Module("A",
      LinearSeq.apply(Import("B", LinearSeq.empty)), LinearSeq.empty)), LinearSeq.apply(True()))))
  }

  @Test def smallDefinitionTest: Unit = {
    val smallDef: String =
      """
         []
         module B
          syntax ASort [\true()]
         endmodule []
      """.stripMargin
    val parsedSimpleDefinition: Definition = MiniKoreParser.parse(smallDef)
    val miniKoreDef = Definition(LinearSeq(
      Module("B", LinearSeq(
        SortDeclaration("ASort", LinearSeq(True()))), LinearSeq.empty))
      , LinearSeq.empty)
    assert(parsedSimpleDefinition.equals(miniKoreDef))
  }
}
