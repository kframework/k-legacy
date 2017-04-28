package org.kframework.backend.java

import org.kframework.kore._
import org.kframework.kore.implementation.DefaultBuilders
import org.kframework.minikore.converters.KoreToMini


object RewriterUtils {

  val b: Builders = DefaultBuilders

  def toLeft(p: Pattern): Pattern = {
    val decodedTuple: (Pattern, Seq[Pattern]) = MiniKoreUtils.decodePatternAttributes(p)
    val lhs = decodedTuple._1 match {
      case Rewrite(left: Pattern, _) => left
      case Application(symbol, items: Seq[Pattern]) => b.Application(symbol, items map toLeft)
      case other => other
    }
    KoreToMini.encodePatternAtt(lhs, b.Attributes(decodedTuple._2))
  }

  def toRight(p: Pattern): Pattern = {
    val decodedTuple: (Pattern, Seq[Pattern]) = MiniKoreUtils.decodePatternAttributes(p)
    val rhs = decodedTuple._1 match {
      case Rewrite(_ , right: Pattern) => right
      case Application(symbol, items: Seq[Pattern]) => b.Application(symbol, items map toRight)
      case other => other
    }
    KoreToMini.encodePatternAtt(rhs, b.Attributes(decodedTuple._2))
  }

}
