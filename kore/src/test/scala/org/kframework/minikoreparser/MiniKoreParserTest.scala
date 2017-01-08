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
          syntax BSort ::= #B(A) []
         endmodule []
      """.stripMargin
    val parsedSimpleDefinition: Definition = MiniKoreParser.parse(smallDef)
    val miniKoreDef = Definition(LinearSeq(
      Module("B", LinearSeq(
        SortDeclaration("ASort", LinearSeq(True())),
        SymbolDeclaration("BSort", "#B", LinearSeq("A"), LinearSeq.empty))
        , LinearSeq.empty)), LinearSeq.empty)
    assert(parsedSimpleDefinition.equals(miniKoreDef))
  }

  @Test def definitionTest: Unit = {
    val patternDef: String =
      """
        []
        module BOOL
          syntax Bool []
          syntax Bool ::= true() []
          axiom \equal(true(), bool("true")) []
          syntax Bool ::= false() []
          axiom \equal(false(), bool("false")) []
          syntax Bool ::= not@BOOL(Bool) []
        endmodule []

      """.replace("\r", "").stripMargin
    val parsedPatternDef : Definition = MiniKoreParser.parse(patternDef)
    val definition : Definition = Definition(LinearSeq(
      Module("BOOL", LinearSeq(
        SortDeclaration("Bool", LinearSeq.empty),
        SymbolDeclaration("Bool", "true", LinearSeq.empty, LinearSeq.empty),
        Axiom(Equal(Application("true", LinearSeq.empty), DomainValue("bool", "\"true\"")), LinearSeq.empty),
        SymbolDeclaration("Bool", "false", LinearSeq.empty, LinearSeq.empty),
        Axiom(Equal(Application("false", LinearSeq.empty), DomainValue("bool", "\"false\"")), LinearSeq.empty),
        SymbolDeclaration("Bool", "not@BOOL", LinearSeq("Bool"), LinearSeq.empty)
      ), LinearSeq.empty)
    ), LinearSeq.empty)
    assert(parsedPatternDef.equals(definition))
  }
}
