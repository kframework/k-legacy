package org.kframework.definition

import org.kframework.frontend
import org.kframework.frontend.ADT
import collection._

trait KLabelMappings {
  self: Module =>

  lazy val labelsToProductions: Map[frontend.KLabel, Set[Production]] =
    sentences collect {
      case prod: Production => (makeKLabel(prod.items), prod)
    } groupBy (_._1) mapValues (_ map { _._2 })

  def makeKLabel(items: Seq[ProductionItem]): frontend.KLabel = ADT.KLabelLookup(
    items map {
      case NonTerminal(sort) => "_"
      case Terminal(string, _) => string
      //TODO(cos): remove this
      case RegexTerminal(_, regex, _) => "regexp"
    } mkString)
}
