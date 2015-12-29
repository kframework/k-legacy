package org.kframework.backend.kore.latex

import org.kframework.definition._
import org.kframework.kore.{Sort, KToken}
import org.kframework.utils.StringUtil

/**
 * Created by Edgar Pek on 10/5/15.
 */

class LatexPatterns(d : Definition) {

  /// map from productions to patterns
  var patterns = scala.collection.mutable.Map[Production, String]()
  private var pattern : String = ""
  private var nonTerm: Int = 0
  private var prevNonTerm: Boolean = false
  private val specialTerminals:Set[String] = Set("(", ")", ",", "[", "]", "{", "}")

  /// create a pattern for each production
  def createPatterns = {
    d.modules.flatMap(m => m.productions).foreach(patternForProduction)
  }

  private def patternForProduction(p: Production): Unit = {
    if (p.att.attMap.get("latex").isDefined) {
      val latexAtt = p.att.attMap.get("latex").get
      assert(latexAtt.klist.size == 1)
      assert(latexAtt.klist.items.get(0).isInstanceOf[KToken])
      pattern = latexAtt.klist.items.get(0).asInstanceOf[KToken].s
    } else {
      pattern = ""
      nonTerm = 1
      prevNonTerm = false
      p.items.foreach(patternForProductionItem)
    }
    patterns.put(p, pattern)
  }

  private def patternForProductionItem(pi : ProductionItem) =
    pi match {
      case NonTerminal(_) | RegexTerminal(_, _, _) =>
        if (prevNonTerm) pattern += "\\mathrel{}"
        pattern += "{#" + nonTerm + "}"
        nonTerm = nonTerm + 1
        prevNonTerm = true
      case tt: Terminal =>
        val terminal = tt.value
        if (specialTerminals.contains(terminal)) {
          pattern += StringUtil.latexify(terminal)
        } else {
          if (!prevNonTerm) pattern += "{}"
          pattern += "\\terminal{" + StringUtil.latexify(terminal) + "}"
        }
      case err =>
        throw new AssertionError("Unsupported production item: " + err.toString)
    }

}
