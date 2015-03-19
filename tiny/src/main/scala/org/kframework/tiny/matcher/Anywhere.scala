package org.kframework.tiny.matcher

import org.kframework.attributes.Att
import org.kframework.builtin.Sorts
import org.kframework.kore.KLabel
import org.kframework.tiny._


case class Anywhere(name: K, k: K, att: Att = Att()) extends KProduct with PlainNormalization {
  val klabel = AnywhereLabel
  override def matcher(right: K): Matcher = AnywhereMatcher(this, right)
  val TOPVariable = KVar(name + ".TOPVariable", Att())
  val HOLEVariable = KVar(name + ".HOLEVariable", Att())
}

import org.kframework.builtin.Sugar._

object Anywhere extends org.kframework.definition.Module("ANYWHERE", Set(), Set(
  Sorts.K ::= "ANYWHERE" + "(" + Sorts.KString + "," + Sorts.K + ")"
)) {
  val label = KLabel("ANYWHERE(_,_)")
  def apply(name: String, k: K): Anywhere = AnywhereLabel(???, k, Att()).asInstanceOf[Anywhere]
  // ADT.KToken(Sorts.String, name, Att())
}

object AnywhereLabel extends KProduct2Label with EmptyAtt {
  override def apply(k1: K, k2: K, att: Att): KProduct = new Anywhere(k1, k2, Att())
  override def delegateLabel: KLabel = Anywhere.label
}

case class AnywhereMatcher(left: Anywhere, right: K) extends Matcher with KProduct {
  override val klabel = AnywhereMatcher
  val TOPVariable = left.TOPVariable
  val HOLEVariable = left.HOLEVariable

  override def normalizeInner(implicit theory: Theory) = {
    val localSolution = And(left.k.matcher(right), Binding(TOPVariable, HOLEVariable))
    val childrenSolutions = right match {
      case k: KApp =>
        Or(k.children.map {c: K => // we are generating distinct solutions for each child of the right
          val solution: K = left.matcher(c).normalize
          val updatedSolution: K = solution match {
            case orSolution: Or => Or(orSolution.children map {
              case s: And => // for each distinct subsolution (a child c may have multiple subsolutions)
                rewire(s, k, c)
            } toSeq: _*)
            case oneSolution: And => rewire(oneSolution, k, c)
          }
          updatedSolution
        }.toSeq: _*)
      case _ => False
    }
    Or(localSolution, childrenSolutions).normalize
  }

  private def rewire(toRewire: And, k: KApp, childOfK: K)(implicit theory: Theory): K = {
    val newAnywhere: K = k.klabel(k.children map {childK: K =>
      childK match {
        case `childOfK` => toRewire.binding(TOPVariable)
        case t: K => t
      }
    } toSeq, k.att)
    val anywhereWrapper = Binding(TOPVariable, newAnywhere)
    val newChildren = toRewire.children.map {
      case b: Binding if b.variable == TOPVariable => anywhereWrapper
      case x => x
    }
    And(newChildren.toSeq: _*).normalize
  }

}

object AnywhereMatcher extends MatcherLabel with KProduct2Label {
  override def apply(k1: K, k2: K, att: Att): KProduct =
    new AnywhereMatcher(k1.asInstanceOf[Anywhere], k2)
}

