package org.kframework.minikore

import org.kframework.attributes.Att
import org.kframework.definition
import org.kframework.attributes

import collection._

object KoreToMini extends (definition.Definition => Definition) {

  implicit def symbolToLabel(s: Symbol): Label = Label(s.name)


  def apply(d: definition.Definition): Definition = d match {
    case definition.Definition(mainModule, entryModules, att) =>
      val modules = entryModules flatMap apply map {
        case m if m.name == mainModule.name => m.copy(att = m.att + 'mainModule ())
        case m => m
      }
      Definition(modules)
  }

  def apply(m: definition.Module): Set[Module] = {
    val trasitiveModules = m.imports flatMap apply
    val importSentences: Set[Sentence] = m.imports map (_.name) map Import
    val localSentences: Set[Sentence] = m.localSentences map apply
    val theCurrentModule = Module(m.name, importSentences | localSentences, apply(m.att))

    trasitiveModules + theCurrentModule
  }

  val nonTerminal = Label("nonTerminal")

  def apply(s: definition.Sentence): Sentence = s match {
    case production@definition.Production(sort, items, atts) =>
      val constructorSorts: Seq[Sort] = items collect {
        case definition.NonTerminal(sort) => Sort(sort.name)
      }
      val encodedSyntax: Seq[Apply] = items map {
        case definition.NonTerminal(sort) => 'nonTerminal ('Sort (sort.name))
        case definition.Terminal(value, follow) =>
          val convertedFollow: Seq[Constant] = follow map (s => 'regex (s))
          'terminal ('string (value), 'seq (convertedFollow: _*))
        case definition.RegexTerminal(precede, regex, follow) =>
          'regexTerminal ('regex (precede), 'regex (regex), 'regex (follow))
      }
      val labelName: String = production.klabel.map(_.name).getOrElse("construct" + production.sort.name)

      ConstructorDeclaration(
        Sort(sort.name),
        Label(labelName),
        constructorSorts,
        apply(atts) ++ encodedSyntax)

    case definition.SyntaxSort(sort, atts) => ???
    //....
  }

  def apply(att: attributes.Att): Att = ???
}
