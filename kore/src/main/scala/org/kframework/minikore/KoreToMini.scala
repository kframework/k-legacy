package org.kframework.minikore

import org.kframework.kore.SortedADT.SortedKVariable
import org.kframework.kore.Unapply._
import org.kframework.kore._
import org.kframework.minikore.MiniKore._
import org.kframework.{attributes, definition}

import scala.collection.JavaConverters._
import scala.collection._

object KoreToMini {

  // Outer

  def apply(d: definition.Definition): Definition = {
    var modules = d.modules.toSeq.map(apply)
    modules = modules.map(m => {
      if (m.name == d.mainModule.name) m.copy(att = m.att :+ Term("mainModule", Seq()))
      else m
    })
    Definition(modules)
  }

  def apply(m: definition.Module): Module = {
    val localSentences: Seq[Sentence] = m.localSentences.toSeq.map(apply)
    val importSentences: Seq[Sentence] = m.imports.toSeq.map(m => Import(m.name))
    Module(m.name, importSentences ++ localSentences, apply(m.att))
  }

  def apply(s: definition.Sentence): Sentence = s match {
    case production @ definition.Production(sort, items, att) =>
      val args = items.collect({
        case definition.NonTerminal(sort) => sort.name
      })
      val newAtt = items.map(encode) ++ apply(att)
      production.klabel match {
        case Some(label) => Syntax(sort.name, label.name, args, newAtt)
        case None => Syntax(sort.name, "", args, newAtt) // TODO(Daejun): generate injection label
      }
    case definition.SyntaxSort(sort, att) => Syntax(sort.name, "", Seq(), apply(att)) // TODO(Daejun): more encoding

    case definition.Rule(body, requires, ensures, att) =>
      val r = apply(requires)
      val b = apply(body)
      val e = apply(ensures)
      val p = Implies(r, And(b, Next(e))) // requires  ->  body  /\  \next ensures
      Rule(p, apply(att))

    case _ => encode(s)
  }

  def apply(att: attributes.Att): Att = {
    att.att.toSeq.map(apply)
  }

  def encode(i: definition.ProductionItem): Pattern = i match {
    case definition.NonTerminal(sort) => Term("NonTerminal", Seq(S(sort.name)))
    case definition.Terminal(value, followRegex) =>
      Term("Terminal", S(value) +: followRegex.map(s => S(s)))
    case definition.RegexTerminal(precedeRegex, regex, followRegex) =>
      Term("RegexTerminal", Seq(S(precedeRegex), S(regex), S(followRegex)))
  }

  def encode(s :definition.Sentence): Sentence = {
    val att = s match {
      case definition.SyntaxPriority(priorities, att) => ???
      case definition.SyntaxAssociativity(assoc, tags, att) => ???
      case definition.ModuleComment(comment, att) => Term("ModuleComment", Seq(S(comment))) +: apply(att)
      case _ => ??? // assert false
    }
    Axiom(B(true), att)
  }

  // Inner

  // Relationship between multiple inner KORE interfaces/implementations
  //
  // destructor  : interface.scala    , Unapply.scala : ADT.scala
  // constructor : Constructors.scala , _             : KORE.scala
  //
  // NOTE: Since interface.scala is defined using traits,
  // its companion object (Unapply.scala) is provided to support pattern matching.

  def apply(k: K): Pattern = {
    val p = k match {
      case kapp: KApply =>
        Term(encode(kapp.klabel), encode(kapp.klist))
      case kvar @ SortedKVariable(name, att) =>
        assert(att == k.att)
        Variable(name, kvar.sort.name)
      case KVariable(name) =>
        // TODO(Daejun): may need to distinguish against SortedKVariable
        apply(SortedKVariable(name, k.att)) // from SortedADT in ADT.scala
      case KToken(s, sort) => ??? // Constant(sort.name, s)
      case KSequence(ks) => ??? // fold
      case KRewrite(left, right) => Rewrite(apply(left), apply(right))
      case InjectedKLabel(klabel) => ???
      case _ => ??? // Sort, KLabel, KList // assert false
    }
    encode(p, k.att)
  }

  def encode(l: KLabel): String = l.name

  def encode(l: KList): Seq[Pattern] = {
    l.items.asScala.toSeq.map(apply) // from KList in Unapply.scala
  }

  def encode(p: Pattern, att: attributes.Att): Pattern = {
    if (att.att.isEmpty) p else ??? // fold
  }

}
