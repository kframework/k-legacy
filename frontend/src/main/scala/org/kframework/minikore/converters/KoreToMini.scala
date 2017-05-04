package org.kframework.minikore.converters

import org.kframework.kore._
import org.kframework.kore.implementation.DefaultBuilders
import org.kframework.frontend.SortedADT.SortedKVariable
import org.kframework.frontend.Unapply._
import org.kframework.frontend._
import org.kframework.{attributes, definition}

import scala.collection._

object KoreToMini {

  // Outer
  val b: Builders = DefaultBuilders

  import b._

  def apply(d: definition.Definition): Definition = {
    val modules: Seq[Module] = d.modules.toSeq.map(apply)
    val att =
      Attributes(Application(iMainModule, Seq(S(d.mainModule.name))) +:
        Application(iEntryModules, d.entryModules.toSeq.map(m => S(m.name))) +:
        apply(d.att))
    Definition(att, modules)
  }

  def apply(m: definition.Module): Module = {
    val localSentences: Seq[Sentence] = m.localSentences.toSeq.map(apply)
    val importSentences: Seq[Sentence] = m.imports.toSeq.map(m => Import(ModuleName(m.name), Attributes(Seq())))
    Module(ModuleName(m.name), importSentences ++ localSentences, Attributes(apply(m.att)))
  }

  def apply(s: definition.Sentence): Sentence = s match {
    case definition.SyntaxSort(sort, att) => SortDeclaration(b.Sort(sort.name), Attributes(apply(att)))

    case prod@definition.Production(sort, items, att) =>
      val args = items.collect({
        case definition.NonTerminal(sort) => b.Sort(sort.name)
      })
      val newAtt = items.map(encode) ++ apply(att)
      prod.klabel match {
        case Some(label) => {
          if (label.name != "") SymbolDeclaration(b.Sort(sort.name), b.Symbol(label.name), args, Attributes(newAtt))
          else {
            val isRegex = items.collect({
              case r@definition.RegexTerminal(_, _, _) => r
            }).nonEmpty
            if(isRegex) {
              SymbolDeclaration(b.Sort(sort.name), b.Symbol(sort.name), args, Attributes(newAtt))
            }
            else {
              SymbolDeclaration(b.Sort(sort.name), iNone, args, Attributes(newAtt))
            }
          }
        }
        case None => SymbolDeclaration(b.Sort(sort.name), iNone, args, Attributes(newAtt)) // TODO(Daejun): either subsort or regex; generate injection label for subsort; dummy sentence for regex
      }

    case definition.Rule(body, requires, ensures, att) =>
      val r = apply(requires)
      val ab = apply(body)
      val e = apply(ensures)
      val p = b.Implies(r, b.And(ab, b.Next(e))) // requires  ->  body  /\  \next ensures
      Rule(p, Attributes(apply(att)))

    case _ => encode(s)
  }

  def apply(att: attributes.Att): Seq[Pattern] = {
    att.att.toSeq.map(apply)
  }

  def encode(i: definition.ProductionItem): Pattern = i match {
    case definition.NonTerminal(sort) =>
      b.Application(iNonTerminal, Seq(S(sort.name)))
    case definition.Terminal(value, followRegex) =>
      b.Application(iTerminal, S(value) +: followRegex.map(s => S(s)))
    case definition.RegexTerminal(precedeRegex, regex, followRegex) =>
      b.Application(iRegexTerminal, Seq(S(precedeRegex), S(regex), S(followRegex)))
  }

  def encode(s: definition.Sentence): Sentence = {
    val p = s match {
      case definition.ModuleComment(comment, _) =>
        b.Application(iModuleComment, Seq(S(comment)))
      case definition.SyntaxPriority(priorities, _) =>
        b.Application(iSyntaxPriority, priorities.map(prio =>
          b.Application(iSyntaxPriorityGroup, prio.toSeq.map(tag => S(tag.name)))
        ))
      case definition.SyntaxAssociativity(assoc, tags, _) =>
        val assocString = assoc match {
          case definition.Associativity.Left => "left"
          case definition.Associativity.Right => "right"
          case definition.Associativity.NonAssoc => "non-assoc"
        }
        b.Application(iSyntaxAssociativity, S(assocString) +: tags.toSeq.map(tag => S(tag.name)))
      case definition.Bubble(sentence, contents, _) => // TODO(Daejun): find why it appears here
        b.Application(iBubble, Seq(S(sentence), S(contents)))
      case definition.Context(body, requires, _) => // TODO(Daejun): may be dropped; context may appear only in non-main modules, which will not be used anyway
        b.Application(iContext, Seq(apply(body), apply(requires)))
      case definition.Configuration(body, ensures, _) => // TODO(Daejun): may not be needed, since configuration is already resolved in the parsing step
        b.Application(iConfiguration, Seq(apply(body), apply(ensures)))
      case _ => ??? // assert false
    }
    dummySentence(Attributes(p +: apply(s.att)))
  }

  def dummySentence(att: Attributes): Sentence = Axiom(B(true), att)

  def S(s: String): Pattern = b.DomainValue(Symbol("S"), Value(s))
  def I(i: Int): Pattern = b.DomainValue(Symbol("I"), Value(i.toString))
  def B(bool: Boolean): Pattern = b.DomainValue(Symbol("B"), Value(bool.toString))

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
      case KApply(klabel, klist) => b.Application(Symbol(klabel.name), klist.map(apply))
      case kvar@SortedKVariable(name, _) => b.SortedVariable(Name(name), b.Sort(kvar.sort.name)) // assert(att == k.att)
      case KVariable(name) => b.SortedVariable(b.Name(name), b.Sort("_")) // TODO(Daejun): apply(SortedKVariable(name, k.att)) // from SortedADT in ADT.scala
      case KToken(s, sort) => b.DomainValue(Symbol(sort.name), Value(s))
      case KSequence(ks) => encodeKSeq(ks.map(apply))
      case KRewrite(left, right) => b.Rewrite(apply(left), apply(right))
      case InjectedKLabel(klabel) => ???
      case _ => ??? // Sort, KLabel, KList // assert false
    }
    encodePatternAtt(p, Attributes(apply(k.att)))
  }

  // encodePatternAtt(p, Seq(a1,a2,a3)) = #(#(#(p,a1),a2),a3) // TODO(Daejun): add test
  def encodePatternAtt(p: Pattern, att: Attributes): Pattern = {
    att.patterns.foldLeft(p)((z, a) => {
      b.Application(iAtt, Seq(z, a))
    })
  }

  // encodeKSeq(Seq(p1,p2,p3)) = #kseq(p1,#kseq(p2,#kseq(p3,#kseqnil))) // TODO(Daejun): add test
  def encodeKSeq(ps: Seq[Pattern]): Pattern = {
    val nil = b.Application(iKSeqNil, Seq())
    ps.reverse.foldLeft(nil)((z, p) => {
      b.Application(iKSeq, Seq(p, z))
    })
  }

  //

  val encodingLabelTuple@(
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
    Symbol("#MainModule"),
    Symbol("#EntryModules"),
    Symbol("#NonTerminal"),
    Symbol("#Terminal"),
    Symbol("#RegexTerminal"),
    Symbol("#ModuleComment"),
    Symbol("#SyntaxPriority"),
    Symbol("#SyntaxPriorityGroup"),
    Symbol("#SyntaxAssociativity"),
    Symbol("#Bubble"),
    Symbol("#Context"),
    Symbol("#Configuration"),
    Symbol("#None"))
  val encodingLabels = encodingLabelTuple.productIterator.toSet

  val iAtt = Symbol("#")
  val iKSeq = Symbol("#kseq")
  val iKSeqNil = Symbol("#kseqnil")

  val iNone = Symbol("#None")
}
