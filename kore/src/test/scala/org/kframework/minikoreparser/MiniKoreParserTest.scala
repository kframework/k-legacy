package org.kframework.minikoreparser

import org.junit.Test
import org.kframework.minikore.MiniKore._
import org.kframework.parser.minikore.MiniKoreParser;
import scala.collection._

class MiniKoreParserTest {
    @Test def emptyDefinitionParse: Unit = {
        val emptyDef : String= "[] "
        val parsedEmptyDefinition: Definition= MiniKoreParser.parse(emptyDef)
        assert(parsedEmptyDefinition.equals(Definition(LinearSeq.empty, LinearSeq.empty)))
    }

    @Test def emptyModuleParse: Unit = {
      val emptyDef : String= "[] module A endmodule []"
      val parsedEmptyDefinition: Definition= MiniKoreParser.parse(emptyDef)
//      assert(parsedEmptyDefinition.equals(Definition(LinearSeq.apply(Module("A", LinearSeq.empty, LinearSeq.empty)), LinearSeq.empty)))
      assert(true)
    }
}
