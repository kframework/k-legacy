package org.kframework.minikoreparser

import org.junit.Test
import org.kframework.minikore.MiniKore.{Definition, Module}

/**
  * Created by manasvi on 1/4/17.
  */
class MiniKoreParserTest {
    @Test def moduleParse: Unit = {
      val moduleString = "module"
      val definition = MiniKoreParser.parse(moduleString);
      assert(true)
    }
}
