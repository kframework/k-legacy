package org.kframework.minikore

import org.kframework.kore.SortedADT.SortedKVariable
import org.kframework.kore.Unapply._
import org.kframework.kore._
import org.kframework.minikore.MiniKore._
import org.kframework.{attributes, definition}
import org.kframework.minikore.KoreToMini._

import scala.collection.JavaConverters._
import scala.collection._

object MiniToKore {

  def apply(d: Definition): definition.Definition = {
    def seq2map(ms: Seq[Module]): Map[String, Module] = {
      ms.groupBy(m => m.name)
        .mapValues( /* ms => ms match */ {
          case Seq(m) => m
          case _ => ??? // shouldn't have duplicate module names
        })
    }
    val origModuleMap: Map[String, Module] = seq2map(d.modules)

    def isMainModule(m: Module): Boolean = m.att match {
      case Term(`iMainModule`, Seq()) +: _ => true
      case _ => false
    }
    val (mainModules, otherModules) = d.modules.partition(isMainModule)
    val mainModule = mainModules.head; assert(mainModules.size == 1)

    val newModuleMapRef: mutable.Map[String, definition.Module] = mutable.Map.empty // will dynamically grow during translating modules
    val newMainModule = apply(origModuleMap,newModuleMapRef)(mainModule)
    val newOtherModules = otherModules.map(apply(origModuleMap,newModuleMapRef))
    definition.Definition(
      newMainModule,
      newOtherModules.toSet + newMainModule, // TODO(Daejun): need to restrict to only entryModules?
      attributes.Att() // TODO(Daejun): need to preserve definition's att?
    )
  }

  def apply(origModuleMap: Map[String, Module], newModuleMapRef: mutable.Map[String, definition.Module])
           (m: Module): definition.Module = {
    val imports = m.sentences.collect({
      case Import(name) => findOrGenModule(origModuleMap,newModuleMapRef)(name)
    })
    val sentences = m.sentences.filter(s => s match {
      case Import(_) => false
      case _ => true
    })
    definition.Module(m.name, imports.toSet, sentences.map(apply).toSet, apply(m.att))
  }

  def findOrGenModule(origModuleMap: Map[String, Module], newModuleMapRef: mutable.Map[String, definition.Module])
                     (name: String): definition.Module = {
    if (newModuleMapRef.contains(name)) newModuleMapRef(name)
    else {
      val m = apply(origModuleMap,newModuleMapRef)(origModuleMap(name))
      newModuleMapRef += (name -> m)
      m
    }
  }

  def apply(s: Sentence): definition.Sentence = s match {
    case Syntax(sort, "", Seq(), att) => definition.SyntaxSort(KORE.Sort(sort), apply(att))
    case Syntax(sort, _, _, att) =>
      val items = att.collect({
        case Term(`iNonTerminal`, Seq(Constant("S", s))) =>
          definition.NonTerminal(KORE.Sort(s))
        case Term(`iTerminal`, Constant("S", value) +: followRegex) =>
          definition.Terminal(value, followRegex.map({case Constant("S", s) => s case _ => ???}))
        case Term(`iRegexTerminal`, Seq(Constant("S", precede), Constant("S", regex), Constant("S", follow))) =>
          definition.RegexTerminal(precede, regex, follow)
      })
      definition.Production(KORE.Sort(sort), items, apply(att))
    case Rule(Implies(r, And(b, Next(e))), att) =>
      definition.Rule(apply(b), apply(r), apply(e), apply(att))
    case Axiom(Constant("B", "true"), att) => decode(att)
    case _ => ??? // assert false
  }

  def decode(att: Att): definition.Sentence = att match {
    case Term(`iModuleComment`, Seq(Constant("S", comment))) +: att =>
      definition.ModuleComment(comment, apply(att))
    case _ => // TODO:
      definition.ModuleComment("", apply(att))
  }

  def apply(att: Att): attributes.Att = {
    def isDummy(p: Pattern): Boolean = p match {
      case Term(l, _) => encodingLabels.contains(l)
      case _ => true
    }
    attributes.Att(att.filterNot(isDummy).map(apply).toSet)
  }

  def apply(p: Pattern): K = apply(attributes.Att())(p)

  def apply(att: attributes.Att)(p: Pattern): K = p match {
    case Term(`iKSeq`, Seq(p1,p2)) =>
      apply(p2) match {
        case k2 @ KSequence(ks) => ADT.KSequence(apply(p1) +: ks.toList, k2.att)
        case _ => ???
      }
    case Term(`iAtt`, Seq(p1,p2)) =>
      val a2 = apply(Seq(p2))
      apply(att ++ a2)(p1)
    case Term(label, args) =>
      KORE.KApply(KORE.KLabel(label), args.map(apply), att)
    case Constant(label, value) =>
      KORE.KToken(value, KORE.Sort(label), att)
    case Variable(name, _) =>
      SortedKVariable(name, att)
    case Rewrite(left, right) =>
      KORE.KRewrite(apply(left), apply(right), att)
    case _ => ???
  }

}
