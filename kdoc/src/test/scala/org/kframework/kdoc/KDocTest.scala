package org.kframework.kdoc

import org.junit._
import Assert._

class KDocTest {
  @Test def simple() {
    assertEquals(
      """
        |\begin{document}  require "domains.k"  begin{module}{ moduleName{ X }  import INT  syntax Foo ::= "x"  rule x => x
        |  end{module} \end{document}
      """.stripMargin.trim
      , new KDoc("")(
        """
        | require "domains.k"
        | module X
        |   imports INT
        |   syntax Foo ::= "x"
        |   rule x => x
        | endmodule
        """.stripMargin).trim)
  }
}
