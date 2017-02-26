package org.kframework.backend.java

import org.kframework.minikore.Build.Builders
import org.kframework.minikore.{DefaultBuilders, KoreToMini}
import org.kframework.minikore.PatternInterface._


object RewriterUtils {

  val b: Builders = DefaultBuilders

  def toLeft(p: Pattern): Pattern = {
    val decodedTuple: (Pattern, Seq[Pattern]) = MiniKoreUtils.decodePatternAttributes(p)
    val lhs = decodedTuple._1 match {
      case Rewrite(left: Pattern, _) => left
      case Application(symbol: String, items: Seq[Pattern]) => b.Application(symbol, items map toLeft)
      case other => other
    }
    KoreToMini.encodePatternAtt(lhs, decodedTuple._2)
  }

  def toRight(p: Pattern): Pattern = {
    val decodedTuple: (Pattern, Seq[Pattern]) = MiniKoreUtils.decodePatternAttributes(p)
    val rhs = decodedTuple._1 match {
      case Rewrite(_ , right: Pattern) => right
      case Application(symbol: String, items: Seq[Pattern]) => b.Application(symbol, items map toRight)
      case other => other
    }
    KoreToMini.encodePatternAtt(rhs, decodedTuple._2)
  }

}
