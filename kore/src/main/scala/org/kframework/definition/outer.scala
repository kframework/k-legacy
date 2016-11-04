// Copyright (c) 2014 K Team. All Rights Reserved.

package org.kframework.definition

import javax.annotation.Nonnull

import dk.brics.automaton.{BasicAutomata, RegExp, RunAutomaton, SpecialOperations}
import org.kframework.POSet
import org.kframework.attributes.Att
import org.kframework.definition.Constructors._
import org.kframework.attributes.{Source, Location, Att}
import org.kframework.kore.SortedADT.SortedKVariable
import org.kframework.kore.Unapply.{KApply, KLabel}
import org.kframework.kore._
import org.kframework.utils.errorsystem.KEMException

import scala.annotation.meta.param
import scala.collection.JavaConverters._

import collection._
import scala.collection.Set

trait OuterKORE

case class NonTerminalsWithUndefinedSortException(nonTerminals: Set[NonTerminal])
  extends AssertionError(nonTerminals.toString())

case class DivergingAttributesForTheSameKLabel(ps: Set[Production])
  extends AssertionError(ps.toString)

//object NonTerminalsWithUndefinedSortException {
//  def apply(nonTerminals: Set[NonTerminal]) =
//    new NonTerminalsWithUndefinedSortException(nonTerminals.toString, nonTerminals)
//
//}

case class Definition(
                       mainModule: Module,
                       entryModules: Set[Module],
                       att: Att = Att())
  extends DefinitionToString with OuterKORE {

  private def allModules(m: Module): Set[Module] = m.imports | (m.imports flatMap allModules) + m

  val modules = entryModules flatMap allModules

  for (m1 <- modules; m2 <- modules if m1.name == m2.name && m1 != m2) {
    throw new AssertionError("In definition, found different modules with the same name: " + m1.name)
  }

  assert(modules.contains(mainModule))

  def getModule(name: String): Option[Module] = modules find { case m: Module => m.name == name; case _ => false }

  override def hashCode = mainModule.hashCode

  override def equals(that: Any) = that match {
    case Definition(`mainModule`, `entryModules`, _) => true
    case _ => false
  }
}

trait Sorting {
  def computeSubsortPOSet(sentences: Set[Sentence]) = {
    val subsortRelations: Set[(Sort, Sort)] = sentences collect {
      case Production(endSort, Seq(NonTerminal(startSort)), att) if !att.contains("klabel") => (startSort, endSort)
    }

    POSet(subsortRelations)
  }
}

trait GeneratingListSubsortProductions extends Sorting {

  def computeFromSentences(wipSentences: Set[Sentence]): Set[Sentence] = {
    val userLists = UserList.apply(wipSentences)

    val subsorts = computeSubsortPOSet(wipSentences)

    val listProductions =
      for (l1 <- userLists;
           l2 <- userLists
           if l1 != l2 && l1.klabel == l2.klabel &&
             subsorts.>(l1.childSort, l2.childSort)) yield {
        Production(l1.sort, Seq(NonTerminal(l2.sort)), Att().add(Att.generatedByListSubsorting))
      }

    listProductions.toSet
  }
}

object Module {
  def apply(name: String, unresolvedLocalSentences: Set[Sentence]): Module = {
    new Module(name, Set(), unresolvedLocalSentences, Att())
  }
}

case class Module(val name: String, val imports: Set[Module], unresolvedLocalSentences: Set[Sentence], @(Nonnull@param) val att: Att = Att())
  extends ModuleToString with KLabelMappings with OuterKORE with Sorting with GeneratingListSubsortProductions with Serializable {
  assert(att != null)

  val unresolvedLocalSyntaxSentences: Set[SyntaxSentence] = unresolvedLocalSentences.collect({ case s: SyntaxSentence => s })
  val unresolvedLocalSemanticSentences: Set[SemanticSentence] = unresolvedLocalSentences.collect({ case s: SemanticSentence => s })

  val lookingToDefineSorts: Set[Sort] = unresolvedLocalSyntaxSentences collect {
    case Production(s, _, _) => s
    case SyntaxSort(s, _) => s
  }

  val sortResolver: SymbolResolver[Sort, ADT.Sort] =
    new SymbolResolver(name, imports map {_.sortResolver}, lookingToDefineSorts)(ADT.SortLookup.apply, ADT.Sort.apply)

  def resolve(s: Sort): ADT.Sort = sortResolver(s)

  private val importedSorts = imports flatMap {
    //noinspection ForwardReference
    _.sorts
  }

  case class OuterException(m: String) extends Throwable {
    override def getMessage() = "While constructing module " + name + ": " + m
  }

  val localSorts: Set[ADT.Sort] = ((lookingToDefineSorts map sortResolver.get).flatten).filter(_.moduleName.s == name)

  def makeTooManySortsErrorMessage(name: String, sortSet: Set[ADT.Sort]): String = {
    "While defining module " + this.name + ": "
    "Found too many sorts named: " + name + ". Possible sorts: " + sortSet.mkString(", ")
  }

  val sorts: Set[ADT.Sort] = localSorts ++ importedSorts

  /**
    * "a@A" -> ("a", "A")
    * "a" -> ("a", this.name) for the local module
    */
  private def splitAtModule(name: String): (String, String) = name.split("@") match {
    case Array(localName, moduleName) => (localName, moduleName)
    case Array(localName) => (localName, this.name)
    case _ => throw KEMException.compilerError("Sort name contains multiple @ symbols: " + name)
  }

  def SortOption(localName: String): Option[ADT.Sort] = sortResolver.get(ADT.SortLookup(localName, ModuleName.STAR))

  def Sort(localName: String): ADT.Sort = SortOption(localName) match {
    case Some(s) => s
    case None => throw KEMException.compilerError("Could not find sort named: " + localName)
  }

  val localSyntaxSentences: Set[SyntaxSentence] = unresolvedLocalSyntaxSentences map {
    case p: Production => p.copy(sort = resolve(p.sort), p.items map {
      case t: NonTerminal => t.copy(sort = resolve(t.sort))
      case other => other
    })
    case s: SyntaxSort => s.copy(sort = resolve(s.sort))
    case other => other
  }

  def resolveSorts(k: K): K = (new TransformK() {
    override def apply(t: KToken): K = t match {
      case tt: ADT.KToken => tt.copy(sort = sortResolver(tt.sort))
    }
  }) (k)

  //    k match {
  //    case app: KApply => app.copy(list map resolveSorts, app.att)
  //  }

  val localSemanticSentences = unresolvedLocalSemanticSentences map {
    case c: Configuration => Configuration(resolveSorts(c.body), resolveSorts(c.ensures), c.att)
    case c: Context => Context(resolveSorts(c.body), resolveSorts(c.requires), c.att)
    case r: Rule => Rule(resolveSorts(r.body), resolveSorts(r.requires), resolveSorts(r.ensures), r.att)
    case b: Bubble => b
  }

  val afterResolvingSorts = localSyntaxSentences ++ localSemanticSentences

  private val importedSentences = imports flatMap {_.sentences}

  val listProductions = computeFromSentences(afterResolvingSorts | importedSentences).diff(importedSentences)

  val localSentences = afterResolvingSorts | listProductions

  val sentences: Set[Sentence] = localSentences | importedSentences

  /** All the imported modules, calculated recursively. */
  lazy val importedModules: Set[Module] = imports | (imports flatMap {
    _.importedModules
  })

  for (m1 <- importedModules; m2 <- importedModules if m1.name == m2.name && m1 != m2) {
    throw new AssertionError("While creating module " + name + " found different modules with the same name: " + m1.name)
  }

  val productions: Set[Production] = sentences collect { case p: Production => p }

  lazy val productionsFor: Map[KLabel, Set[Production]] =
    productions
      .collect({ case p if p.klabel != None => p })
      .groupBy(_.klabel.get)
      .map { case (l, ps) => (l, ps) }

  lazy val productionsForSort: Map[Sort, Set[Production]] =
    productions
      .groupBy(_.sort)
      .map { case (l, ps) => (l, ps) }

  @transient
  lazy val attForSort: Map[Sort, Att] =
    productionsForSort mapValues {_ map {_.att} reduce {_.++(_)}}

  @transient
  lazy val definedKLabels: Set[KLabel] =
    (productionsFor.keys.toSet | klabelsDefinedInRules.keys.toSet).filter(!_.isInstanceOf[KVariable])

  lazy val klabelsDefinedInRules: Map[KLabel, Int] = {
    def mergeMultiset(map1: Map[KLabel, Int], map2: Map[KLabel, Int]) = map1 ++ map2.map { case (k, v) => k -> (v + map1.getOrElse(k, 0)) }

    val transformer = new FoldK[Map[KLabel, Int]] {
      override def apply(k: KApply): Map[KLabel, Int] = merge(apply(k.klist), Map((k.klabel, 1)))

      override def apply(k: InjectedKLabel): Map[KLabel, Int] = Map((k.klabel, 1))

      def unit = Map()

      def merge(map1: Map[KLabel, Int], map2: Map[KLabel, Int]) = mergeMultiset(map1, map2)
    }
    rules.map(r => {
      mergeMultiset(transformer.apply(r.body), mergeMultiset(transformer.apply(r.requires), transformer.apply(r.ensures)))
    }).fold(Map())(mergeMultiset)
  }

  lazy val tokenProductionsFor: Map[Sort, Set[Production]] =
    productions
      .collect({ case p if p.att.contains("token") => p })
      .groupBy(_.sort)
      .map { case (s, ps) => (s, ps) }

  lazy val bracketProductionsFor: Map[Sort, List[Production]] =
    productions
      .collect({ case p if p.att.contains("bracket") => p })
      .groupBy(_.sort)
      .map { case (s, ps) => (s, ps.toList.sortBy(_.sort)(subsorts.asOrdering)) }

  @transient lazy val sortFor: Map[KLabel, Sort] = productionsFor mapValues {_.head.sort}

  def optionSortFor(k: K): Option[Sort] = k match {
    case Unapply.KApply(l, _) => sortFor.get(l)
    case Unapply.KRewrite(_, r) => optionSortFor(r)
    case Unapply.KToken(_, sort) => Some(sort)
    case Unapply.KSequence(s) => optionSortFor(s.last)
  }

  def isSort(klabel: KLabel, s: Sort) = subsorts.<(sortFor(klabel), s)

  lazy val rules: Set[Rule] = sentences collect { case r: Rule => r }

  lazy val localRules: Set[Rule] = localSentences collect { case r: Rule => r }

  // Check that productions with the same klabel have identical attributes
  //  productionsFor.foreach {
  //    case (l, ps) =>
  //      if (ps.groupBy(_.att).size != 1)
  //        throw DivergingAttributesForTheSameKLabel(ps)
  //  }

  @transient lazy val attributesFor: Map[KLabel, Att] = productionsFor mapValues {mergeAttributes(_)}

  @transient lazy val signatureFor: Map[KLabel, Set[(Seq[Sort], Sort)]] =
    productionsFor mapValues {
      ps: Set[Production] =>
        ps.map {
          p: Production =>
            val params: Seq[Sort] = p.items collect { case NonTerminal(sort) => sort }
            (params, p.sort)
        }
    }

  val sortDeclarations: Set[SyntaxSort] = sentences.collect({ case s: SyntaxSort => s })

  lazy val sortDeclarationsFor: Map[Sort, Set[SyntaxSort]] =
    sortDeclarations
      .groupBy(_.sort)

  @transient lazy val sortAttributesFor: Map[Sort, Att] = sortDeclarationsFor mapValues {mergeAttributes(_)}

  private def mergeAttributes[T <: Sentence](p: Set[T]) = {
    val union = p.flatMap(_.att.att)
    val attMap = union.collect({ case t@KApply(KLabel(_), _) => t }).groupBy(_.klabel)
    Att(union.filter { k => !k.isInstanceOf[KApply] || attMap(k.asInstanceOf[KApply].klabel).size == 1 })
  }

  val definedSorts: Set[Sort] = (productions map {_.sort}) ++ (sortDeclarations map {_.sort})

  val withSameShortName = definedSorts.groupBy(_.asInstanceOf[ADT.Sort].localName).filter(_._2.size > 1)

  val usedCellSorts: Set[Sort] = productions.flatMap { p => p.items.collect { case NonTerminal(s) => s }
    .filter(s => s.name.endsWith("Cell") || s.name.endsWith("CellFragment"))
  }

  lazy val listSorts: Set[Sort] = sentences.collect({ case Production(srt, _, att1) if att1.contains("userList") =>
    srt
  })

  lazy val subsorts: POSet[Sort] = computeSubsortPOSet(sentences)

  private lazy val expressedPriorities: Set[(Tag, Tag)] =
    sentences
      .collect({ case SyntaxPriority(ps, _) => ps })
      .map { ps: Seq[Set[Tag]] =>
        val pairSetAndPenultimateTagSet = ps.foldLeft((Set[(Tag, Tag)](), Set[Tag]())) {
          case ((all, prev), current) =>
            val newPairs = for (a <- prev; b <- current) yield (a, b)

            (newPairs | all, current)
        }
        pairSetAndPenultimateTagSet._1 // we're only interested in the pair set part of the fold
      }.flatten
  lazy val priorities = POSet(expressedPriorities)
  lazy val leftAssoc = buildAssoc(Associativity.Left)
  lazy val rightAssoc = buildAssoc(Associativity.Right)

  private def buildAssoc(side: Associativity.Value): Set[(Tag, Tag)] = {
    sentences
      .collect({ case SyntaxAssociativity(`side` | Associativity.NonAssoc, ps, _) => ps })
      .map { ps: Set[Tag] =>
        for (a <- ps; b <- ps) yield (a, b)
      }.flatten
  }

  @transient lazy val freshFunctionFor: Map[Sort, KLabel] =
    productions.groupBy(_.sort).mapValues(_.filter(_.att.contains("freshGenerator")))
      .filter(_._2.nonEmpty).mapValues(_.map(p => p.klabel.get)).mapValues { set => {
      if (set.size > 1)
        throw KEMException.compilerError("Found more than one fresh generator for sort " + sortFor(set.head)
          + ". Found: " + set)
      else
        set.head
    }
    }

  // check that non-terminals have a defined sort
  private val nonTerminalsWithUndefinedSort = sentences flatMap {
    case p@Production(_, items, _) =>
      val res = items collect { case nt: NonTerminal if !definedSorts.contains(nt.sort) && !usedCellSorts.contains(nt.sort) => nt }
      if (!res.isEmpty)
        throw KEMException.compilerError("Could not find sorts: " + res.asJava, p)
      res
    case _ => Set()
  }
  if (!nonTerminalsWithUndefinedSort.isEmpty)
    throw new NonTerminalsWithUndefinedSortException(nonTerminalsWithUndefinedSort)

  override lazy val hashCode: Int = name.hashCode

  override def equals(that: Any) = that match {
    case m: Module => m.name == name && m.sentences == sentences
  }
}

// hooked but different from core, Import is a sentence here

trait Sentence {
  // marker
  val att: Att
}

trait SyntaxSentence extends Sentence

trait SemanticSentence extends Sentence

// deprecated
case class Context(body: K, requires: K, att: Att = Att()) extends SemanticSentence with OuterKORE with ContextToString

case class Rule(body: K, requires: K, ensures: K, att: Att = Att()) extends SemanticSentence with RuleToString with OuterKORE

case class ModuleComment(comment: String, att: Att = Att()) extends SyntaxSentence with OuterKORE

// hooked

// syntax declarations

case class SyntaxPriority(priorities: Seq[Set[Tag]], att: Att = Att())
  extends SyntaxSentence with SyntaxPriorityToString with OuterKORE

object Associativity extends Enumeration {
  type Value1 = Value
  val Left, Right, NonAssoc, Unspecified = Value
}

case class SyntaxAssociativity(
                                assoc: Associativity.Value,
                                tags: Set[Tag],
                                att: Att = Att())
  extends SyntaxSentence with SyntaxAssociativityToString with OuterKORE

case class Tag(name: String) extends TagToString with OuterKORE

//trait Production {
//  def sort: Sort
//  def att: Att
//  def items: Seq[ProductionItem]
//  def klabel: Option[KLabel] =
//    att.get(Production.kLabelAttribute).headOption map { case KList(KToken(s, _, _)) => s } map { KLabel(_) }
//}

case class SyntaxSort(sort: Sort, att: Att = Att()) extends SyntaxSentence
  with SyntaxSortToString with OuterKORE {
  def items = Seq()
}

case class Production(sort: Sort, items: Seq[ProductionItem], att: Att)
  extends SyntaxSentence with ProductionToString {
  lazy val klabel: Option[KLabel] = att.get[String]("klabel") map org.kframework.kore.KORE.KLabel

  override def equals(that: Any) = that match {
    case p@Production(`sort`, `items`, _) => this.klabel == p.klabel
    case _ => false
  }

  override lazy val hashCode: Int = (sort.hashCode() * 31 + items.hashCode()) * 31 + klabel.hashCode()

  def isSyntacticSubsort: Boolean =
    items.size == 1 && items.head.isInstanceOf[NonTerminal]

  def arity: Int = items.count(_.isInstanceOf[NonTerminal])

  def nonterminal(i: Int): NonTerminal = items.filter(_.isInstanceOf[NonTerminal])(i).asInstanceOf[NonTerminal]
}

object Production {
  def apply(klabel: String, sort: Sort, items: Seq[ProductionItem], att: Att = Att()): Production = {
    Production(sort, items, att + (kLabelAttribute -> klabel))
  }

  val kLabelAttribute = "klabel"
}

// hooked but problematic, see kast-core.k

sealed trait ProductionItem extends OuterKORE

// marker

sealed trait TerminalLike extends ProductionItem {
  def pattern: RunAutomaton

  def followPattern: RunAutomaton

  def precedePattern: RunAutomaton
}

case class NonTerminal(sort: Sort) extends ProductionItem
  with NonTerminalToString

case class RegexTerminal(precedeRegex: String, regex: String, followRegex: String) extends TerminalLike with
  RegexTerminalToString {
  lazy val pattern = new RunAutomaton(new RegExp(regex).toAutomaton, false)
  lazy val followPattern = new RunAutomaton(new RegExp(followRegex).toAutomaton, false)
  lazy val precedePattern = {
    val unreversed = new RegExp(precedeRegex).toAutomaton
    SpecialOperations.reverse(unreversed)
    new RunAutomaton(unreversed, false)
  }
}

object Terminal {
  def apply(value: String): Terminal = Terminal(value, Seq())
}

case class Terminal(value: String, followRegex: Seq[String]) extends TerminalLike // hooked
  with TerminalToString {
  def this(value: String) = this(value, Seq())

  lazy val pattern = new RunAutomaton(BasicAutomata.makeString(value), false)
  lazy val followPattern =
    new RunAutomaton(BasicAutomata.makeStringUnion(followRegex.toArray: _*), false)
  lazy val precedePattern = new RunAutomaton(BasicAutomata.makeEmpty(), false)
}

/* Helper constructors */
object NonTerminal {
  def apply(sort: String): NonTerminal = NonTerminal(ADT.SortLookup(sort))
}
