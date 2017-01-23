package org.kframework.backend.java

import org.kframework.minikore.KoreToMini
import org.kframework.minikore.MiniKore._


object RewriterUtils {

  def toLeft(p: Pattern): Pattern = {
    val decodedTuple: (Pattern, Seq[Pattern]) = MiniKoreUtils.decodePatternAttributes(p)
    val lhs = decodedTuple._1 match {
      case Rewrite(left: Pattern, _) => left
      case Application(symbol: String, items: Seq[Pattern]) => Application(symbol, items map toLeft)
      case other => other
    }
    KoreToMini.encodePatternAtt(lhs, decodedTuple._2)
  }

  def toRight(p: Pattern): Pattern = {
    val decodedTuple: (Pattern, Seq[Pattern]) = MiniKoreUtils.decodePatternAttributes(p)
    val rhs = decodedTuple._1 match {
      case Rewrite(_ , right: Pattern) => right
      case Application(symbol: String, items: Seq[Pattern]) => Application(symbol, items map toRight)
      case other => other
    }
    KoreToMini.encodePatternAtt(rhs, decodedTuple._2)
  }

}
