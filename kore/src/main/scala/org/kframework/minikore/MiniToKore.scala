package org.kframework.minikore

import org.kframework.kore.SortedADT.SortedKVariable
import org.kframework.kore.KORE._
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
        .mapValues({ case Seq(m) => m; case _ => ??? }) // shouldn't have duplicate module names
    }
    val origModuleMap: Map[String, Module] = seq2map(d.modules)

    val mainModuleName = findAtt(d.att, iMainModule) match {
      case Seq(DomainValue("S", name)) => name; case _ => ???
    }
    val (mainModules, otherModules) = d.modules.partition(m => m.name == mainModuleName)
    val mainModule = mainModules.head; assert(mainModules.size == 1)

    val entryModules = findAtt(d.att, iEntryModules).map({
      case DomainValue("S", name) => origModuleMap(name); case _ => ???
    })

    val newModuleMapRef: mutable.Map[String, definition.Module] = mutable.Map.empty // will dynamically grow during translating modules
    val newMainModule = apply(origModuleMap,newModuleMapRef)(mainModule)
    val newEntryModules = entryModules.map(apply(origModuleMap,newModuleMapRef))
    definition.Definition(
      newMainModule,
      newEntryModules.toSet,
      apply(d.att)
    )
  }

  def apply(origModuleMap: Map[String, Module], newModuleMapRef: mutable.Map[String, definition.Module])
           (m: Module): definition.Module = {
    val imports = m.sentences.collect({
      case Import(name, _) => findOrGenModule(origModuleMap,newModuleMapRef)(name)
    })
    val sentences = m.sentences.filter({ case Import(_, _) => false; case _ => true })
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
    case SortDeclaration(sort, att) => definition.SyntaxSort(Sort(sort), apply(att))
    case SymbolDeclaration(sort, _, _, att) =>
      val items = att.collect({
        case Application(`iNonTerminal`, Seq(DomainValue("S", s))) =>
          definition.NonTerminal(Sort(s))
        case Application(`iTerminal`, DomainValue("S", value) +: followRegex) =>
          definition.Terminal(value, followRegex.map({ case DomainValue("S", s) => s; case _ => ??? }))
        case Application(`iRegexTerminal`, Seq(DomainValue("S", precede), DomainValue("S", regex), DomainValue("S", follow))) =>
          definition.RegexTerminal(precede, regex, follow)
      })
      definition.Production(Sort(sort), items, apply(att))
    case Rule(Implies(r, And(b, Next(e))), att) =>
      definition.Rule(apply(b), apply(r), apply(e), apply(att))
    case Axiom(DomainValue("B", "true"), att) => decode(att)
    case _ => ??? // assert false
  }

  def decode(att: Attributes): definition.Sentence = att match {
    case Application(`iModuleComment`, Seq(DomainValue("S", comment))) +: att =>
      definition.ModuleComment(comment, apply(att))
    case Application(`iSyntaxPriority`, prios) +: att =>
      val priorities = prios.map({
        case Application(`iSyntaxPriorityGroup`, group) =>
          group.map({
            case DomainValue("S", tag) => definition.Tag(tag); case _ => ???
          }).toSet
        case _ => ???
      })
      definition.SyntaxPriority(priorities, apply(att))
    case Application(`iSyntaxAssociativity`, DomainValue("S", assocString) +: tags) +: att =>
      val assoc = assocString match {
        case "left" => definition.Associativity.Left
        case "right" => definition.Associativity.Right
        case "non-assoc" => definition.Associativity.NonAssoc
        case _ => ???
      }
      val ts = tags.map({
        case DomainValue("S", tag) => definition.Tag(tag); case _ => ???
      }).toSet
      definition.SyntaxAssociativity(assoc, ts, apply(att))
    case Application(`iBubble`, Seq(DomainValue("S", sentence), DomainValue("S", contents))) +: att =>
      definition.Bubble(sentence, contents, apply(att))
    case Application(`iContext`, Seq(body, requires)) +: att =>
      definition.Context(apply(body), apply(requires), apply(att))
    case _ => ???
  }

  def apply(att: Attributes): attributes.Att = {
    def isDummy(p: Pattern): Boolean = p match {
      case Application(l, _) => encodingLabels.contains(l); case _ => false
    }
    attributes.Att(att.filterNot(isDummy).map(apply).toSet)
  }

  def apply(p: Pattern): K = apply(attributes.Att())(p)

  def apply(att: attributes.Att)(p: Pattern): K = p match {
    case Application(`iKSeq`, Seq(p1,p2)) =>
      apply(p2) match {
        case k2: KSequence =>
          val items = apply(p1) +: k2.items.asScala.toList // from KSequence in Unapply.scala
          ADT.KSequence(items, att ++ k2.att)
        case _ => ???
      }
    case Application(`iKSeqNil`, Seq()) =>
      ADT.KSequence(List(), att)

    case Application(`iAtt`, Seq(p1,p2)) =>
      val a2 = apply(Seq(p2))
      apply(att ++ a2)(p1)

    case Application(label, args) => KApply(KLabel(label), args.map(apply), att)
    case DomainValue(label, value) => KToken(value, Sort(label), att)
    case Variable(name, "_") => KVariable(name, att)
    case Variable(name, _) => SortedKVariable(name, att)
    case Rewrite(left, right) => KRewrite(apply(left), apply(right), att)
    case _ => ???
  }

  def findAtt(att: Attributes, key: String): Seq[Pattern] = {
    val argss = att.collect({
      case Application(`key`, args) => args
    })
    assert(argss.size == 1)
    argss.head
  }

}
