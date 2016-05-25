package org.kframework.kale

import java.util
import java.util.Optional

import collection.JavaConverters._
import org.kframework.RewriterResult
import org.kframework.attributes.Att
import org.kframework.definition._
import org.kframework.kil.{Attribute, Attributes}
import org.kframework.kore.Unapply.Sort
import org.kframework.kore._
import org.kframework.rewriter.{Rewriter, SearchType}

import collection._

object KaleRewriter {
  val self = this

  def apply(m: Module): KaleRewriter = new KaleRewriter(m)

  private def isEffectivelyAssoc(att: Att): Boolean =
    att.contains(Att.assoc) && !att.contains(Att.assoc) || att.contains(Att.bag)

  val hooks: Map[String, Label] = Map({
    "INT.Int" -> INT
  })
}

class KaleRewriter(m: Module) extends org.kframework.rewriter.Rewriter {

  private val productionLike = m.sentences.collect({
    case p: Production => p
    case s: SyntaxSort => s
  })

  private val assocProductions = productionLike.filter(p => KaleRewriter.isEffectivelyAssoc(p.att))
  private val nonAssocProductions = productionLike &~ assocProductions

  private val nonAssocLabels: Set[Label] = nonAssocProductions flatMap {
    case SyntaxSort(s, att) => att.get(Att.hook) flatMap KaleRewriter.hooks.get
    case p@Production(s, items, att) =>
      att.get(Att.hook).flatMap(KaleRewriter.hooks.get).orElse({
        if (att.contains(Att.token)) {
          Some(org.kframework.kale.STRING)
        } else {
          if(p.klabel.isDefined) {
            val nonTerminals = items.filter(_.isInstanceOf[NonTerminal])
            Some(nonTerminals match {
              case Seq() => FreeLabel0(UniqueId(), p.klabel.get.name)
              case Seq(_) => FreeLabel1(UniqueId(), p.klabel.get.name)
              case Seq(_, _) => FreeLabel2(UniqueId(), p.klabel.get.name)
              case Seq(_, _, _) => FreeLabel3(UniqueId(), p.klabel.get.name)
              case Seq(_, _, _, _) => FreeLabel4(UniqueId(), p.klabel.get.name)
            })
          } else
            None
        }
      })
  }

  private val assocLabels: Set[Label] = assocProductions map { case p @ Production(s, items, att) =>
    val units = nonAssocLabels.filter(l => l.name == p.att.get[String]("unit").get)
    assert(units.size == 1)
    val theUnit = units.head.asInstanceOf[FreeLabel0]()
    AssocWithIdListLabel(p.klabel.get.name, theUnit)
  } toSet

  private val allLabels = nonAssocLabels | assocLabels

  val unifier = SimpleMatcher(allLabels)
  val substitutionApplier = ApplySubstitution(allLabels)
  val rewriterConstructor = Rewriter(substitutionApplier, unifier) _

  def convert(body: K): Term = body match {
    case Unapply.KToken(s, sort) => ???
  }

  val rules = m.rules map {
    case Rule(body, requires, ensures, att) => And(convert(body), convert(requires))
  }

  override def execute(k: K, depth: Optional[Integer]): RewriterResult = {
    new RewriterResult(Optional.of(0), k)
  }

  override def `match`(k: K, rule: Rule): util.List[_ <: util.Map[_ <: KVariable, _ <: K]] = ???

  override def search(initialConfiguration: K, depth: Optional[Integer], bound: Optional[Integer], pattern: Rule, searchType: SearchType): util.List[_ <: util.Map[_ <: KVariable, _ <: K]] = ???

  override def executeAndMatch(k: K, depth: Optional[Integer], rule: Rule): (RewriterResult, util.List[_ <: util.Map[_ <: KVariable, _ <: K]]) = ???

  override def prove(rules: util.List[Rule]): util.List[K] = ???
}
