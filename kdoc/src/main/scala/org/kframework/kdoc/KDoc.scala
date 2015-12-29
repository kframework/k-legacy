package org.kframework.kdoc

import org.kframework.Definition
import org.kframework.Parser
import org.kframework.attributes.Att
import org.kframework.definition.{RegexTerminal, NonTerminal, Terminal, Module}
import org.kframework.kore.{KORE, K}
import org.kframework.kore.Unapply._

class KDoc(docStyle: String, separator: String = " ") {
  private val definitionForEKORE = Definition.from("require \"e-kore.k\"", "E-KORE")
  private val outerParser = Parser.from(definitionForEKORE.mainModule)
  private var endl: String = System.getProperty("line.separator")
  var header = "\\nonstopmode" + endl + "\\PassOptionsToPackage{pdftex,usenames,dvipsnames,svgnames,x11names}{xcolor}" + endl + "\\PassOptionsToPackage{pdftex}{hyperref}" + endl + "\\documentclass{article}" + endl + "\\usepackage[" + docStyle + "]{k}" + endl

  val kToLatexForEKORE = new KtoLatex(definitionForEKORE.mainModule, separator)

  def apply(definitionText: String): String = {
    val d = parseDefinition(definitionText)
    kToLatexForEKORE(d)
  }

  private def parseDefinition(definitionText: String): K = {
    outerParser.apply(KORE.Sort("KDefinition"), definitionText)._1.get
  }
}
