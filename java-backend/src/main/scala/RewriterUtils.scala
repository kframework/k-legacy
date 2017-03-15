package org.kframework.backend.java

import org.kframework.minikore.converters.KoreToMini
import org.kframework.minikore.interfaces.build.Builders
import org.kframework.minikore.implementation.DefaultBuilders
import org.kframework.minikore.interfaces.pattern._


object RewriterUtils {

  val b: Builders = DefaultBuilders

  def toLeft(p: Pattern): Pattern = {
    val decodedTuple: (Pattern, Seq[Pattern]) = MiniKoreUtils.decodePatternAttributes(p)
    val lhs = decodedTuple._1 match {
      case Rewrite(left: Pattern, _) => left
      case Application(symbol, items: Seq[Pattern]) => b.Application(symbol, items map toLeft)
      case other => other
    }
    KoreToMini.encodePatternAtt(lhs, decodedTuple._2)
  }

  def toRight(p: Pattern): Pattern = {
    val decodedTuple: (Pattern, Seq[Pattern]) = MiniKoreUtils.decodePatternAttributes(p)
    val rhs = decodedTuple._1 match {
      case Rewrite(_ , right: Pattern) => right
      case Application(symbol, items: Seq[Pattern]) => b.Application(symbol, items map toRight)
      case other => other
    }
    KoreToMini.encodePatternAtt(rhs, decodedTuple._2)
  }

}
