package org.kframework.kale

import java.util
import java.util.Optional

import org.kframework.RewriterResult
import org.kframework.attributes.Att
import org.kframework.builtin.{KLabels, Sorts}
import org.kframework.definition._
import org.kframework.kore._
import org.kframework.rewriter.SearchType

import collection._
import org.kframework.kale
import org.kframework.kore.SortedADT.SortedKVariable
import org.kframework.kore.Unapply.KRewrite

object KaleRewriter {
  val self = this

  def apply(m: Module): KaleRewriter = new KaleRewriter(m)

  private def isEffectivelyAssoc(att: Att): Boolean =
    att.contains(Att.assoc) || att.contains(Att.bag)
}

class KaleRewriter(m: Module) extends org.kframework.rewriter.Rewriter {

  private val productionWithUniqueSorts: Set[Sentence] = (m.sentences.collect({
    case p: Production if p.klabel.isDefined => p
  }) groupBy {_.klabel.get} map {_._2.head}).toSet

  private val syntaxSortDeclarations: Set[Sentence] = m.sentences.collect({
    case p: SyntaxSort => p
  }) toSet

  private val assocProductions: Set[Sentence] = productionWithUniqueSorts.filter(p => KaleRewriter.isEffectivelyAssoc(p.att)).toSet

  println(assocProductions.size)

  private val nonAssocProductions = (productionWithUniqueSorts | syntaxSortDeclarations) &~ assocProductions

  implicit val env = Environment()

  import env._
  import env.builtin._

  val hooks: Map[String, Label] = Map(

  )

  def relativeHook(relativeHook: String): Option[Label] = relativeHook.split('.').toSeq match {
    case Seq(baseLabel: String, hookName: String) => relativeHookByBaseLabel(baseLabel, hookName)
    case _ => None
  }

  def relativeHookByBaseLabel(baseLabel: String, hookName: String): Option[Label] = env.label(baseLabel) match {
    case l: MapLabel => hookName match {
      case "lookup" => Some(l.lookup)
      case _ => None
    }
    case _ => None
  }

  private val nonAssocLabels: Set[Label] = nonAssocProductions flatMap {
    case SyntaxSort(s, att) => att.get(Att.hook) flatMap hooks.get
    case p@Production(s, items, att) if !att.contains(Att.relativeHook) =>
      implicit val envv = env
      att.get(Att.hook).flatMap(hooks.get).orElse({
        if (att.contains(Att.token)) {
          if (!env.labels.exists(l => l.name == "TOKEN_" + s.name))
            Some(GENERIC_TOKEN(Sort(s.name)))
          else
            None
        } else {
          if (p.klabel.isDefined) {
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
    case _ => None
  }

  private val uninterpretedTokenLabels: Map[Sort, ConstantLabel[String]] = (nonAssocLabels collect {
    case l@GENERIC_TOKEN(s) => (s, l)
  }).toMap + (Sort("KConfigVar@BASIC-K") -> GENERIC_TOKEN(Sort("KConfigVar@BASIC-K")))

  private val nonConstantLabels: Map[String, NodeLabel] = nonAssocLabels collect {
    case l: NodeLabel => (l.name, l)
  } toMap

  private val emptyKSeq = FreeLabel0(".")(env)()
  private val kseq = new AssocWithIdListLabel("~>", emptyKSeq)

  def getLabelForAtt(p: Production, att: String): Label = {
    val labels = nonAssocLabels.filter(l => l.name == p.att.get[String](att).get)
    assert(labels.size == 1)
    labels.head
  }

  assocProductions map {
    case p@Production(s, items, att) =>
      val theUnit = getLabelForAtt(p, "unit").asInstanceOf[Label0]
      val labelName = p.klabel.get.name
      val index = att.get[String]("index")
      if (index.isDefined && att.contains(Att.comm)) {
        def indexFunction(t: Term): Term = t.iterator().toList(index.get.toInt)
        MapLabel(labelName, indexFunction, theUnit())(env)
      } else
        new AssocWithIdListLabel(labelName, theUnit())(env)
  }

  nonAssocProductions collect {
    case p@Production(s, items, att) if att.contains(Att.relativeHook) =>
      relativeHook(att.get(Att.relativeHook).get)
  }

  env.seal()

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
    case Unapply.KApply(klabel, list) if klabel.name == "#KSequence" => kseq(list map convert)
    case Unapply.KApply(klabel, list) => env.label(klabel.name).asInstanceOf[NodeLabel](list map convert)
    case v: KVariable => Variable(v.name)
    case r: KRewrite => Rewrite(convert(r.left), convert(r.right))
  }

  val rules = m.rules map {
    case rule@Rule(KRewrite(l, r), requires, ensures, att) =>
      println(rule, att)
      if (att.contains(Att.`macro`)) {
        ???
        Rewrite(AnywhereContext(Variable("CONTEXT"), And(convert(l), Equality(convert(requires)), BOOLEAN(true))),
          AnywhereContext(Variable("CONTEXT"), convert(r)))
      } else {
        l match {
          case t@Unapply.KApply(klabel, _) if m.attributesFor(klabel).contains(Att.`Function`) =>
            Rewrite(AnywhereContext(Variable("CONTEXT"), And(convert(l), Equality(convert(requires), BOOLEAN(true)))),
              AnywhereContext(Variable("CONTEXT"), convert(r)))
          case _ => Rewrite(And(convert(l), Equality(convert(requires), BOOLEAN(true))), convert(r))
        }
      }
  }

  println(rules.mkString("\n"))

  val rewrite = rewriterConstructor(rules)

  def convertBack(term: Term): K = {
    ???
  }

  override def execute(k: K, depth: Optional[Integer]): RewriterResult = {
    var i = 0
    var term: Term = null
    var next = convert(k)
    do {
      term = next
      println("step " + i + " : " + next)
      next = rewrite.executionStep(term)
      i += 1
    } while (term != next)
    new RewriterResult(Optional.of(0), convertBack(term))
  }

  override def `match`(k: K, rule: Rule): util.List[_ <: util.Map[_ <: KVariable, _ <: K]] = ???

  override def search(initialConfiguration: K, depth: Optional[Integer], bound: Optional[Integer], pattern: Rule, searchType: SearchType): util.List[_ <: util.Map[_ <: KVariable, _ <: K]] = ???

  override def executeAndMatch(k: K, depth: Optional[Integer], rule: Rule): (RewriterResult, util.List[_ <: util.Map[_ <: KVariable, _ <: K]]) = ???

  override def prove(rules: util.List[Rule]): util.List[K] = ???
}
