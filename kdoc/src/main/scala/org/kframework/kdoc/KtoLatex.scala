package org.kframework.kdoc

import org.kframework.Definition
import org.kframework.Parser
import org.kframework.attributes.Att
import org.kframework.definition.{RegexTerminal, NonTerminal, Terminal, Module}
import org.kframework.kore.{KORE, K}
import org.kframework.kore.Unapply._

class KtoLatex(module: Module, separator: String = " ") {
  def apply(k: K): String = k match {
    case KApply(l, children) =>
      val latexAtts = module.productionsFor(l).flatMap(_.att.get[String](Att.latex))
      val latex = latexAtts.size match {
        case 0 => // no latex annotation
          val possibleLatexes = module.productionsFor(l).map(_.items.foldLeft((Seq[String](), 1)) {
            case ((r, i), t: Terminal) => (r :+ t.value, i)
            case ((r, i), t: RegexTerminal) => (r :+ t.regex, i) //TODO: we probably want something better here
            case ((r, i), nt: NonTerminal) => (r :+ "#" + i, i + 1)
          }).map(_._1.mkString(separator))
          possibleLatexes.size match {
            case 0 => throw new AssertionError("Could not find a label for " + l)
            case 1 => possibleLatexes.head
            case _ => throw new AssertionError("Too productions for klabel " + l)
          }
        case 1 => // exactly one latex annotation
          latexAtts.head
        case _ => // multiple latex attributes
          throw new AssertionError("Too many latex attributes for klabel " + l)
      }
      children.zipWithIndex.foldRight(latex) { case ((value, i), res) => res.replaceAll("#" + (i + 1), apply(value)) }
    case KToken(s, _) => s
    case KRewrite(l, r) => apply(l) + "\\Rightarrow" + apply(r) //TODO: use kast.k latex annotation
    case KSequence(s) => s map apply mkString "~>"              //TODO: use kast.k latex annotation
  }
}
