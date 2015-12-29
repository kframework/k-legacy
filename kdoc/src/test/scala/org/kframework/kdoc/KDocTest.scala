package org.kframework.kdoc

import org.junit._
import Assert._

class KDocTest {
  @Test def simple() {
    assertEquals("", new KDoc("")("module X endmodule"))
  }
}
