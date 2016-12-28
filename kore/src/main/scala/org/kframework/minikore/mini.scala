package org.kframework.minikore

import collection._

case class Definition(modules: Set[Module])
case class Module(name: String, sentences: Set[Sentence], att: Att)

case class Label(name: String) {
  def apply(l: Pattern*) = Apply(this, l.toSeq)
  def apply(s: String) = Constant(this, s)
}
case class Sort(name: String)

sealed trait Sentence
case class ConstructorDeclaration(sort: Sort, label: Label, children: Seq[Sort], att: Att) extends Sentence
case class Rule(pattern: Pattern, att: Att) extends Sentence
case class Axiom(pattern: Pattern, att: Att) extends Sentence
case class Import(name: String) extends Sentence
case class Att(atts: Set[Pattern]) {
  def +(p: Pattern) = Att(atts + p)
  def ++(ps: Seq[Pattern]) = Att(atts ++ ps)
}

sealed trait Pattern
case class Variable(name: String, sort: Sort) extends Pattern
case class And(a: Pattern, b: Pattern) extends Pattern
case class Or(a: Pattern, b: Pattern) extends Pattern
case class Neg(p: Pattern) extends Pattern
case class Exists(v: Variable, b: Pattern) extends Pattern
case class ForAll(v: Variable, b: Pattern) extends Pattern
case class Next(p: Pattern) extends Pattern

case class Constant(id: Label, value: String) extends Pattern
case class Apply(id: Label, children: Seq[Pattern]) extends Pattern