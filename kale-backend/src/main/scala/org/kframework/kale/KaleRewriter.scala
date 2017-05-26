package org.kframework.kale

import java.util
import java.util.Optional

import org.kframework.attributes.Att
import org.kframework.backend.skala.SkalaBackend
import org.kframework.definition._
import org.kframework.frontend.Unapply.KRewrite
import org.kframework.frontend._
import org.kframework.rewriter.SearchType
import org.kframework.{RewriterResult, frontend}
import org.kframework.kore
import org.kframework.kore.Pattern
import org.kframework.minikore.converters.{KoreToMini, MiniToKore}
import org.kframework.kore.extended.implicits._
import org.kframework.kore.implementation.{DefaultBuilders => db}

import scala.collection._



class KaleRewriter(mainModule: Module, koreDefinition: kore.Definition) extends org.kframework.rewriter.Rewriter {

  val mainKoreModule = koreDefinition.modulesMap(db.ModuleName(mainModule.name))

  val skalaBackend = SkalaBackend(koreDefinition, mainKoreModule)

  val frontendToKore = KoreToMini

  val koreToFrontend = MiniToKore

  override def execute(k: K, depth: Optional[Integer]): RewriterResult = {
    val koreK = frontendToKore(k)

    val result: Pattern = skalaBackend.step(koreK)

    new RewriterResult(depth, koreToFrontend(k))
  }

  override def `match`(k: K, rule: Rule): K = ???

  override def search(initialConfiguration: K, depth: Optional[Integer], bound: Optional[Integer], pattern: Rule, searchType: SearchType, resultsAsSubstitution: Boolean): K = ???

  override def executeAndMatch(k: K, depth: Optional[Integer], rule: Rule): (RewriterResult, K) = ???

  override def prove(rules: java.util.List[Rule]): java.util.List[K] = ???
}

//object KaleRewriter {
//  val self = this
//
//  def apply(m: Module): KaleRewriter = new KaleRewriter(m)
//
//  private def isEffectivelyAssoc(att: Att): Boolean =
//    att.contains(Att.assoc) || att.contains(Att.bag)
//}
//
//class KaleRewriter(m: Module) extends org.kframework.rewriter.Rewriter {
//
//  private val productionWithUniqueKLabel: Set[Sentence] = (m.sentences.collect({
//    case p: Production if p.klabel.isDefined => p
//  }) groupBy {_.klabel.get} map {_._2.head}).toSet
//
//  private val syntaxSortDeclarations: Set[Sentence] = m.sentences.collect({
//    case p: SyntaxSort => p
//  }) toSet
//
//  private val assocProductions: Set[Sentence] = productionWithUniqueKLabel.filter(p => KaleRewriter.isEffectivelyAssoc(p.att)).toSet
//
//  private val nonAssocProductions = (productionWithUniqueKLabel | syntaxSortDeclarations) &~ assocProductions
//
//  implicit val env = Environment()
//
//  import env._
//  import env.builtin._
//
//  val converters = new KaleConverters(m)
//
//  import converters._
//
//  case class IsSort(s: Sort)(implicit val env: Environment) extends Named("is" + s.name) with PurelyFunctionalLabel1 with FormulaLabel {
//    def f(_1: Term): Option[Term] =
//      kSortOf(_1).map(ss => BOOLEAN(m.subsorts.<=(ss, m.resolve(frontend.KORE.Sort(s.name)))))
//  }
//
//  private val nonAssocLabels: Set[Label] = nonAssocProductions flatMap {
//    case SyntaxSort(s, att) => None
//    case p@Production(s, items, att) if !att.contains(Att.relativeHook) =>
//      implicit val envv = env
//      att.get(Att.hook)
//        .flatMap({ hookName: String => Hook(hookName, p.klabel.get.name) }) // use the hook if there
//        .orElse({
//        if (att.contains(Att.token)) {
//          // it is a token, but not hooked
//          if (!env.labels.exists(l => l.name == "TOKEN_" + s.name))
//            Some(GENERIC_TOKEN(Sort(s.name)))
//          else
//            None
//        } else {
//          if (p.klabel.isDefined) {
//            val nonTerminals = items.filter(_.isInstanceOf[NonTerminal])
//            val label = if (att.contains(Att.Function)) {
//              if (p.klabel.get.name.startsWith("is")) {
//                IsSort(Sort(p.klabel.get.name.substring(2)))
//              } else {
//                nonTerminals match {
//                  case Seq() => FunctionDefinedByRewritingLabel0(p.klabel.get.name)(env)
//                  case Seq(_) => FunctionDefinedByRewritingLabel1(p.klabel.get.name)(env)
//                  case Seq(_, _) => FunctionDefinedByRewritingLabel2(p.klabel.get.name)(env)
//                  case Seq(_, _, _) => FunctionDefinedByRewritingLabel3(p.klabel.get.name)(env)
//                  case Seq(_, _, _, _) => FunctionDefinedByRewritingLabel4(p.klabel.get.name)(env)
//                }
//              }
//            } else {
//              nonTerminals match {
//                case Seq() => FreeLabel0(p.klabel.get.name)
//                case Seq(_) => FreeLabel1(p.klabel.get.name)
//                case Seq(_, _) => FreeLabel2(p.klabel.get.name)
//                case Seq(_, _, _) => FreeLabel3(p.klabel.get.name)
//                case Seq(_, _, _, _) => FreeLabel4(p.klabel.get.name)
//              }
//            }
//            Some(label)
//          } else
//            None
//        }
//      })
//    case _ => None
//  }
//
//  private val nonConstantLabels: Map[String, NodeLabel] = nonAssocLabels collect {
//    case l: NodeLabel => (l.name, l)
//  } toMap
//
//  def getLabelForAtt(p: Production, att: String): Label = {
//    val labels = nonAssocLabels.filter(l => l.name == p.att.get[String](att).get)
//    assert(labels.size == 1)
//    labels.head
//  }
//
//  assocProductions foreach {
//    case p@Production(s, items, att) =>
//      val unitLabel = getLabelForAtt(p, "unit").asInstanceOf[Label0]
//      val opLabelName = p.klabel.get.name
//      env.uniqueLabels.getOrElse(opLabelName, {
//        val index = att.get[String]("index")
//        if (index.isDefined && att.contains(Att.comm)) {
//          def indexFunction(t: Term): Term = t.iterator().toList(index.get.toInt)
//          MapLabel(opLabelName, indexFunction, unitLabel())(env)
//        } else {
//          new AssocWithIdListLabel(opLabelName, unitLabel())(env)
//        }
//      })
//  }
//
//  nonAssocProductions collect {
//    case p@Production(s, items, att) if att.contains(Att.relativeHook) =>
//      Hook.relativeHook(att.get(Att.relativeHook).get)(env)
//  }
//
//  val functionRulesAsLeftRight: Set[(Label, Rewrite)] = m.rules collect {
//    case rule@Rule(KRewrite(l@Unapply.KApply(klabel, _), r), requires, ensures, att)
//      if m.attributesFor(klabel).contains(Att.`Function`) =>
//      val res = (env.label(klabel.name), Rewrite(And(convert(l), Equality(convert(requires), BOOLEAN(true))), convert(r)))
//      res
//  }
//
//  val functionRules: Map[Label, Set[Rewrite]] = functionRulesAsLeftRight groupBy (_._1) map { case (k, set) => (k, set.map(_._2)) }
//
//  val functionRulesWithRenamedVariables: Map[Label, Set[Rewrite]] = functionRules map { case (k, v) => (k, v map env.renameVariables) }
//
//  env.seal()
//
//  def setFunctionRules(functionRules: Map[Label, Set[Rewrite]]) {
//    env.labels.collect({
//      // TODO: Add an warning for when a function is not defined by either a hook or rules
//      case l: FunctionDefinedByRewriting => l.setRules(functionRules.getOrElse(l, Set()))
//    })
//  }
//
//  setFunctionRules(functionRulesWithRenamedVariables)
//
//  def reconstruct(inhibitForLabel: Label)(t: Term): Term = t match {
//    case Node(label, children) if label != inhibitForLabel => label(children map reconstruct(inhibitForLabel) toIterable)
//    case t => t
//  }
//
//  def resolveFunctionRHS(functionRules: Map[Label, Set[Rewrite]]): Map[Label, Set[Rewrite]] = {
//    functionRules map { case (label, rewrites) => (label, rewrites map (rw => reconstruct(label)(rw).asInstanceOf[Rewrite])) } toMap
//  }
//
//  val finalFunctionRules = Util.fixpoint(resolveFunctionRHS)(functionRules)
//  setFunctionRules(finalFunctionRules)
//
//  val rules: Set[Rewrite] = m.rules collect {
//    case rule@Rule(KRewrite(l@Unapply.KApply(klabel, _), r), requires, ensures, att)
//      if !att.contains(Att.`macro`) && !m.attributesFor(klabel).contains(Att.`Function`) =>
//      val rw = Rewrite(And(convert(l), Equality(convert(requires), BOOLEAN(true))), convert(r))
//      rw
//  }
//
//  val matcher = Matcher(env).default
//  val substitutionApplier = SubstitutionApply(env)
//  val rewriterConstructor = Rewriter(substitutionApplier, matcher, env) _
//
//  // TODO: log this information cleanly
//  //  println("\nFunction rules\n")
//  //  finalFunctionRules.foreach({ case (l, rules) => println(l); println(rules.map("  " + _).mkString("\n")) })
//  //  println("\nRewriting rules\n")
//  //  println(rules.mkString("\n"))
//
//  val rewrite = rewriterConstructor(rules)
//
//  override def execute(k: K, depth: Optional[Integer]): RewriterResult = {
//    var i = 0
//    var term: Term = null
//    var next: Term = convert(k)
//    while (term != next && depth.map[Boolean](i == _).orElse(true)) {
//      term = next
//      next = rewrite.step(term).headOption.getOrElse(term)
//      i += 1
//    }
//    term = next
//    new RewriterResult(Optional.of(0), convertBack(term))
//  }
//
//  override def `match`(k: K, rule: Rule): K = {
//    val kaleO = convert(k)
//    val kaleRule = rule match {
//      case rule@Rule(KRewrite(l@Unapply.KApply(klabel, _), r), requires, ensures, att) =>
//        val rw = Rewrite(And(convert(l), Equality(convert(requires), BOOLEAN(true))), convert(r))
//        rw
//    }
//    val res = matcher(kaleRule._1, kaleO)
//    convertBack(res)
//  }
//
//
//  override def search(initialConfiguration: K, depth: Optional[Integer], bound: Optional[Integer], pattern: Rule, searchType: SearchType, resultsAsSubstitution: Boolean): K = ???
//
//  override def executeAndMatch(k: K, depth: Optional[Integer], rule: Rule): (RewriterResult, K) = ???
//
//  override def prove(rules: util.List[Rule]): util.List[K] = ???
//}
