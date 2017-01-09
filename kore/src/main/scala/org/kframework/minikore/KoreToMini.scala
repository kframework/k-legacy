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
    val modules = d.modules.toSeq.map(apply)
    val att =
      Application(iMainModule, Seq(S(d.mainModule.name))) +:
      Application(iEntryModules, d.entryModules.toSeq.map(m => S(m.name))) +:
      apply(d.att)
    Definition(modules, att)
  }

  def apply(m: definition.Module): Module = {
    val localSentences: Seq[Sentence] = m.localSentences.toSeq.map(apply)
    val importSentences: Seq[Sentence] = m.imports.toSeq.map(m => Import(m.name, Seq()))
    Module(m.name, importSentences ++ localSentences, apply(m.att))
  }

  def apply(s: definition.Sentence): Sentence = s match {
    case definition.SyntaxSort(sort, att) => SortDeclaration(sort.name, apply(att))

    case prod @ definition.Production(sort, items, att) =>
      val args = items.collect({
        case definition.NonTerminal(sort) => sort.name
      })
      val newAtt = items.map(encode) ++ apply(att)
      prod.klabel match {
        case Some(label) => SymbolDeclaration(sort.name, label.name, args, newAtt)
        case None => SymbolDeclaration(sort.name, "", args, newAtt) // TODO(Daejun): either subsort or regex; generate injection label for subsort; dummy sentence for regex
      }

    case definition.Rule(body, requires, ensures, att) =>
      val r = apply(requires)
      val b = apply(body)
      val e = apply(ensures)
      val p = Implies(r, And(b, Next(e))) // requires  ->  body  /\  \next ensures
      Rule(p, apply(att))

    case _ => encode(s)
  }

  def apply(att: attributes.Att): Attributes = {
    att.att.toSeq.map(apply)
  }

  def encode(i: definition.ProductionItem): Pattern = i match {
    case definition.NonTerminal(sort) =>
      Application(iNonTerminal, Seq(S(sort.name)))
    case definition.Terminal(value, followRegex) =>
      Application(iTerminal, S(value) +: followRegex.map(s => S(s)))
    case definition.RegexTerminal(precedeRegex, regex, followRegex) =>
      Application(iRegexTerminal, Seq(S(precedeRegex), S(regex), S(followRegex)))
  }

  def encode(s :definition.Sentence): Sentence = {
    val p = s match {
      case definition.ModuleComment(comment, _) =>
        Application(iModuleComment, Seq(S(comment)))
      case definition.SyntaxPriority(priorities, _) =>
        Application(iSyntaxPriority , priorities.map(prio =>
          Application(iSyntaxPriorityGroup, prio.toSeq.map(tag => S(tag.name)))
        ))
      case definition.SyntaxAssociativity(assoc, tags, _) =>
        val assocString = assoc match {
          case definition.Associativity.Left => "left"
          case definition.Associativity.Right => "right"
          case definition.Associativity.NonAssoc => "non-assoc"
        }
        Application(iSyntaxAssociativity, S(assocString) +: tags.toSeq.map(tag => S(tag.name)))
      case definition.Bubble(sentence, contents, _) => // TODO(Daejun): find why it appears here
        Application(iBubble, Seq(S(sentence), S(contents)))
      case definition.Context(body, requires, _) => // TODO(Daejun): may be dropped; context may appear only in non-main modules, which will not be used anyway
        Application(iContext, Seq(apply(body), apply(requires)))
      case definition.Configuration(body, ensures, _) => // TODO(Daejun): may not be needed, since configuration is already resolved in the parsing step
        Application(iConfiguration, Seq(apply(body), apply(ensures)))
      case _ => ??? // assert false
    }
    dummySentence(p +: apply(s.att))
  }

  def dummySentence(att: Attributes): Sentence = Axiom(B(true), att)

  def S(s: String): DomainValue = DomainValue("S", s)
  def I(i: Int): DomainValue = DomainValue("I", i.toString)
  def B(b: Boolean): DomainValue = DomainValue("B", b.toString)

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
      case KApply(klabel, klist) => Application(klabel.name, klist.map(apply))
      case kvar @ SortedKVariable(name, _) => Variable(name, kvar.sort.name) // assert(att == k.att)
      case KVariable(name) => Variable(name, "_") // TODO(Daejun): apply(SortedKVariable(name, k.att)) // from SortedADT in ADT.scala
      case KToken(s, sort) => DomainValue(sort.name, s)
      case KSequence(ks) => encodeKSeq(ks.map(apply))
      case KRewrite(left, right) => Rewrite(apply(left), apply(right))
      case InjectedKLabel(klabel) => ???
      case _ => ??? // Sort, KLabel, KList // assert false
    }
    encodePatternAtt(p, apply(k.att))
  }

  // encodePatternAtt(p, Seq(a1,a2,a3)) = #(#(#(p,a1),a2),a3) // TODO(Daejun): add test
  def encodePatternAtt(p: Pattern, att: Attributes): Pattern = {
    att.foldLeft(p)((z,a) => {
      Application(iAtt, Seq(z,a))
    })
  }

  // encodeKSeq(Seq(p1,p2,p3)) = #kseq(p1,#kseq(p2,#kseq(p3,#kseqnil))) // TODO(Daejun): add test
  def encodeKSeq(ps: Seq[Pattern]): Pattern = {
    val nil = Application(iKSeqNil, Seq())
    ps.reverse.foldLeft(nil)((z,p) => {
      Application(iKSeq, Seq(p,z))
    })
  }

  //

  val encodingLabelTuple @ (
    iMainModule,
    iEntryModules,
    iNonTerminal,
    iTerminal,
    iRegexTerminal,
    iModuleComment,
    iSyntaxPriority,
    iSyntaxPriorityGroup,
    iSyntaxAssociativity,
    iBubble,
    iContext,
    iConfiguration,
  _) = (
    "#MainModule",
    "#EntryModules",
    "#NonTerminal",
    "#Terminal",
    "#RegexTerminal",
    "#ModuleComment",
    "#SyntaxPriority",
    "#SyntaxPriorityGroup",
    "#SyntaxAssociativity",
    "#Bubble",
    "#Context",
    "#Configuration",
  "")
  val encodingLabels = encodingLabelTuple.productIterator.toSet

  val iAtt = "#"
  val iKSeq = "#kseq"
  val iKSeqNil = "#kseqnil"

}
