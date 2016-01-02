package org.kframework.kdoc

import org.junit._
import Assert._

class KDocTest {
  @Test def simple() {
    assertEquals(
      """
        |\begin{document}  require "domains.k"  begin{module}{ moduleName{ X }  import INT  syntax Foo ::= "x"  syntax Foo ::= "y" [ latex ( "specialLatexForY" ) ] rule x => specialLatexForY end{module} \end{document}
      """.stripMargin.trim
      , new KDoc("")(
        """
        | require "domains.k"
        | module X
        |   imports INT
        |   syntax Foo ::= "x"
        |   syntax Foo ::= "y" [latex("specialLatexForY")]
        |   rule x => y requires x ensures y
        |   context x requires y
        | endmodule
        """.stripMargin).trim)
  }
}
