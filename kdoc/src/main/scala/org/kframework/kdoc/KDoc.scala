package org.kframework.kdoc

import org.kframework.{DefinitionParser, Kompiler, Parser}
import org.kframework.frontend.KApply
import org.kframework.frontend.KLabel
import org.kframework.frontend.KORE._
import org.kframework.frontend.KToken
import org.kframework.frontend.Sort
import org.kframework.frontend.Unapply.KApply
import org.kframework.frontend.Unapply.KLabel
import org.kframework.frontend.Unapply.KToken
import org.kframework.frontend.Unapply.Sort
import org.kframework.definition._
import org.kframework.frontend._
import org.kframework.frontend.Unapply._
import org.kframework.parser.concrete2kore.generator.RuleGrammarGenerator

class KDoc(docStyle: String, separator: String = " ") {
  private val definitionForEKORE = DefinitionParser.from("require \"e-kore.k\"", "E-KORE")
  private val outerParser = Parser.from(definitionForEKORE.mainModule)
  private var endl: String = System.getProperty("line.separator")
  var header = "\\nonstopmode" + endl + "\\PassOptionsToPackage{pdftex,usenames,dvipsnames,svgnames,x11names}{xcolor}" + endl + "\\PassOptionsToPackage{pdftex}{hyperref}" + endl + "\\documentclass{article}" + endl + "\\usepackage[" + docStyle + "]{k}" + endl

  val kToLatexForEKORE = new KtoLatex(definitionForEKORE.mainModule, separator)

  def apply(definitionText: String): String = {
    val metaDefinition = parseDefinition(definitionText)
    val definition = DefinitionParser.from(definitionText)
    val resolvedConfigs = Kompiler.configurationSentencesToSyntaxAndRules(definition)
    val andAddedTheRuleParsingModules = Kompiler.toRuleParser(resolvedConfigs)
    val res = kToLatexForEKORE(resolveBubbles(metaDefinition, andAddedTheRuleParsingModules))
    res.replace("\\n", System.getProperty("line.separator"))
  }

  def resolveBubbles(k: K, definition: org.kframework.definition.Definition) = new TransformK() {
    var currentModule: Module = null
    var currentParser: Parser = null
    var currentKToLatex: KtoLatex = null

    override def apply(k: KApply) = k match {
      case m@KApply(KLabel("#KModule"), KToken(moduleName, _) +: _) =>
        currentModule = definition.getModule(moduleName + "-" + RuleGrammarGenerator.RULE_CELLS).get
        currentParser = Parser.from(currentModule)
        currentKToLatex = new KtoLatex(currentModule, separator)
        super.apply(m)
      case o => super.apply(o)
    }

    override def apply(k: KToken): K = k match {
      case c@KToken(contents, Sort("Bubble")) =>
        currentParser(ADT.Sort("RuleContent", ModuleName("REQUIRES-ENSURES")), contents) match {
          case (Some(parsedBubble), _) => currentKToLatex(parsedBubble)
          case (_, errs) => throw new RuntimeException("When parsing: "+contents + " got errors:\n"+errs.toString)
        }
      case other => super.apply(other)
    }
  }.apply(k)

  private def parseDefinition(definitionText: String): K = {
    outerParser.apply(KORE.Sort("KDefinition", ModuleName("OUTER-KORE")), definitionText) match {
      case (Some(x), _) => x
      case (None, errs) => throw new RuntimeException(errs.toString)
    }
  }
}
