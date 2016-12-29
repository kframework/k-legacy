package org.kframework.minikore

import collection._

import org.kframework.attributes.Att
import org.kframework.definition
import org.kframework.attributes

import org.kframework.minikore.MiniKore._

object KoreToMini {

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
      val encodedSyntax: Seq[Pattern] = items.map({
        case definition.NonTerminal(sort) => Term("NonTerminal", Seq(S(sort.name)))
        case definition.Terminal(value, followRegex) =>
          Term("Terminal", S(value) +: followRegex.map(s => S(s)))
        case definition.RegexTerminal(precedeRegex, regex, followRegex) =>
          Term("RegexTerminal", Seq(S(precedeRegex), S(regex), S(followRegex)))
      })
      val labelName: String = production.klabel.map(_.name).getOrElse("construct" + production.sort.name)
      Syntax(sort.name, labelName, args, apply(att) ++ encodedSyntax)

    case definition.SyntaxSort(sort, att) => ???
    case rule: definition.Rule => ???
    case _: definition.SyntaxPriority => ???
    case _: definition.SyntaxAssociativity => ???
    case _: definition.ModuleComment => ???
  }

  def apply(att: attributes.Att): MiniKore.Att = {
    Seq()
    // att.att.toSeq.map(???)
  }
}
