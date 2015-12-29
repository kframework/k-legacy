package org.kframework.kdoc

import org.kframework.Definition
import org.kframework.Parser
import org.kframework.attributes.Att
import org.kframework.definition.{RegexTerminal, NonTerminal, Terminal, Module}
import org.kframework.kore._
import org.kframework.kore.Unapply._

class KDoc(docStyle: String, separator: String = " ") {
  private val definitionForEKORE = Definition.from("require \"e-kore.k\"", "E-KORE")
  private val outerParser = Parser.from(definitionForEKORE.mainModule)
  private var endl: String = System.getProperty("line.separator")
  var header = "\\nonstopmode" + endl + "\\PassOptionsToPackage{pdftex,usenames,dvipsnames,svgnames,x11names}{xcolor}" + endl + "\\PassOptionsToPackage{pdftex}{hyperref}" + endl + "\\documentclass{article}" + endl + "\\usepackage[" + docStyle + "]{k}" + endl

  val kToLatexForEKORE = new KtoLatex(definitionForEKORE.mainModule, separator)

  def apply(definitionText: String): String = {
    val metaDefinition = parseDefinition(definitionText)
    val definition = Definition.from(definitionText)
    kToLatexForEKORE(resolveBubbles(metaDefinition, definition))
  }

  def resolveBubbles(k: K, definition: org.kframework.definition.Definition) = new TransformK() {
    var currentModule: Module = null
    var currentParser: Parser = null
    var currentKToLatex: KtoLatex = null

    override def apply(k: KApply) = k match {
      case m@KApply(KLabel("#KModule"), KToken(moduleName, _) +: _) =>
        currentModule = definition.getModule(moduleName).get
        currentParser = Parser.from(currentModule)
        currentKToLatex = new KtoLatex(currentModule, separator)
        super.apply(m)
      case o => super.apply(o)
    }

    override def apply(k: KToken) = k match {
      case c@KToken(contents, Sort("BubbleItem")) =>
//        val parsedBubble = currentParser(c)
//        currentKToLatex(parsedBubble)
        c
      case other => other
    }
  }.apply(k)

  private def parseDefinition(definitionText: String): K = {
    outerParser.apply(KORE.Sort("KDefinition"), definitionText)._1.get
  }
}
