package org.kframework.kale

import java.util
import java.util.Optional

import org.kframework.RewriterResult
import org.kframework.attributes.Att
import org.kframework.builtin.Sorts
import org.kframework.definition._
import org.kframework.kore._
import org.kframework.rewriter.SearchType

import collection._
import org.kframework.kale
import org.kframework.kore.SortedADT.SortedKVariable

object KaleRewriter {
  val self = this

  def apply(m: Module): KaleRewriter = new KaleRewriter(m)

  private def isEffectivelyAssoc(att: Att): Boolean =
    att.contains(Att.assoc) && !att.contains(Att.assoc) || att.contains(Att.bag)

  val hooks: Map[String, Label] = Map()
}

class KaleRewriter(m: Module) extends org.kframework.rewriter.Rewriter {

  private val productionLike: Set[Sentence] = (m.sentences.collect({
    case p: Production => (p.klabel, p)
    case s: SyntaxSort => (s.sort, s)
  }) groupBy { _._1 } map { _._2.head } values).toSet

  private val assocProductions = productionLike.filter(p => KaleRewriter.isEffectivelyAssoc(p.att))
  private val nonAssocProductions = productionLike &~ assocProductions

  implicit val env = Environment()
  import env._
  import env.builtin._

  private val nonAssocLabels: Set[Label] = nonAssocProductions flatMap {
    case SyntaxSort(s, att) => att.get(Att.hook) flatMap KaleRewriter.hooks.get
    case p@Production(s, items, att) =>
      implicit val envv = env
      att.get(Att.hook).flatMap(KaleRewriter.hooks.get).orElse({
        if (att.contains(Att.token)) {
          Some(GENERIC_TOKEN(Sort(s.name)))
        } else {
          if(p.klabel.isDefined) {
            val nonTerminals = items.filter(_.isInstanceOf[NonTerminal])
            Some(nonTerminals match {
              case Seq() => FreeLabel0(p.klabel.get.name)
              case Seq(_) => FreeLabel1(p.klabel.get.name)
              case Seq(_, _) => FreeLabel2(p.klabel.get.name)
              case Seq(_, _, _) => FreeLabel3(p.klabel.get.name)
              case Seq(_, _, _, _) => FreeLabel4(p.klabel.get.name)
            })
          } else
            None
        }
      })
  }

  private val uninterpretedTokenLabels: Map[Sort, ConstantLabel[String]] = nonAssocLabels collect {
    case  l@GENERIC_TOKEN(s) => (s, l)
  } toMap

  private val nonConstantLabels: Map[String, NodeLabel] = nonAssocLabels collect {
    case  l: NodeLabel => (l.name, l)
  } toMap

  private val assocLabels: Set[Label] = assocProductions map { case p @ Production(s, items, att) =>
    val units = nonAssocLabels.filter(l => l.name == p.att.get[String]("unit").get)
    assert(units.size == 1)
    val theUnit = units.head.asInstanceOf[FreeLabel0]()
    new AssocWithIdListLabel(p.klabel.get.name, theUnit)(env)
  } toSet

  private val allLabels = nonAssocLabels | assocLabels

  val unifier = Matcher(env)
  val substitutionApplier = SubstitutionApply(env)
  val rewriterConstructor = Rewriter(substitutionApplier, unifier, env) _

  def convert(body: K): Term = body match {
    case Unapply.KToken(s, sort) => sort match {
      case Sorts.Bool => BOOLEAN(s.toBoolean)
      case Sorts.Int => INT(s.toInt)
      case Sorts.String => STRING(s)
      case _ => uninterpretedTokenLabels(Sort(sort.name))(s)
    }
    case Unapply.KApply(klabel, list) => env.label(klabel.name).asInstanceOf[NodeLabel](list map convert)
    case v: SortedKVariable => Variable(v.name)
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
