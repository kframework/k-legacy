package org.kframework.kdoc

import org.junit._
import Assert._

class KDocTest {
  @Test def simple() {
    assertEquals("", KDoc("module X endmodule"))
  }
}
