package org.kframework.kdoc

import org.junit._
import Assert._

class KDocTest {
  @Test def simple() {
    assertEquals(
      """
        |\begin{document}   begin{module}{ moduleName{ X }  import Y  syntax Foo ::= "x"  end{module} \end{document}
      """.stripMargin.trim
      , new KDoc("")(
        """
        | module X
        |   imports Y
        |   syntax Foo ::= "x"
        | endmodule
        """.stripMargin).trim)
  }
}
