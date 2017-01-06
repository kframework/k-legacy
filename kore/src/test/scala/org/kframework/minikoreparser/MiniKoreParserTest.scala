package org.kframework.minikoreparser

import org.junit.Test
import org.kframework.minikore.MiniKore._
import org.kframework.parser.minikore.MiniKoreParser;
import scala.collection._

class MiniKoreParserTest {
    @Test def moduleParse: Unit = {
        val emptyDef = "[] "
        val parsedEmptyDefinition: Definition= MiniKoreParser.parse(emptyDef)
        assert(parsedEmptyDefinition.equals(Definition(LinearSeq.empty, LinearSeq.empty)))
    }
}
