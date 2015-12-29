package org.kframework.kdoc

import org.kframework.Definition
import org.kframework.Parser
import org.kframework.attributes.Att
import org.kframework.definition.{RegexTerminal, NonTerminal, Terminal, Module}
import org.kframework.kore.{KORE, K}
import org.kframework.kore.Unapply._

class KDoc(docStyle: String) {
  private val definitionForEKore = Definition.from("require \"e-kore.k\"", "E-KORE")
  private val parser = Parser.from(definitionForEKore.mainModule)
  private var endl: String = System.getProperty("line.separator")
  var header = "\\nonstopmode" + endl + "\\PassOptionsToPackage{pdftex,usenames,dvipsnames,svgnames,x11names}{xcolor}" + endl + "\\PassOptionsToPackage{pdftex}{hyperref}" + endl + "\\documentclass{article}" + endl + "\\usepackage[" + docStyle + "]{k}" + endl

  def apply(definitionText: String): String = {
    val d = parseDefinition(definitionText)
    List(header, "\\begin{document}", latexify(d), "\\end{document}").mkString(endl)
  }

  def latexify(k: K): String = k match {
    case KApply(KLabel("#KDefinition"), children) => ""
  }

  private def parseDefinition(definitionText: String): K = {
    parser.apply(KORE.Sort("KDefinition"), definitionText)._1.get
  }
}


