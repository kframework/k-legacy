package org.kframework.definition

import org.kframework.legacykore
import org.kframework.legacykore.ADT
import collection._

trait KLabelMappings {
  self: Module =>

  lazy val labelsToProductions: Map[legacykore.KLabel, Set[Production]] =
    sentences collect {
      case prod: Production => (makeKLabel(prod.items), prod)
    } groupBy (_._1) mapValues (_ map { _._2 })

  def makeKLabel(items: Seq[ProductionItem]): legacykore.KLabel = ADT.KLabelLookup(
    items map {
      case NonTerminal(sort) => "_"
      case Terminal(string, _) => string
      //TODO(cos): remove this
      case RegexTerminal(_, regex, _) => "regexp"
    } mkString)
}
