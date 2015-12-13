package org.kframework.kore

import org.kframework.builtin.KLabels
import org.kframework.kore
import org.kframework.attributes._
import org.kframework.utils.errorsystem.KEMException
import collection.JavaConverters._
import org.kframework.definition.{LookupSymbol, ModuleName, ResolvedSymbol, Module}

/**
  * Abstract Data Types: basic implementations for the inner KORE interfaces.
  *
  * Tools using inner KORE data structures can either use these classes directly or have their own implementations.
  */


object ADT {

  case class KLabelLookup(name: String) extends kore.KLabel {
    override def toString = name

    def apply(ks: K*) = KApply(this, KList(ks.toList))
  }

  case class KLabel(module: Module, localName: String) extends kore.KLabel {
    val name = localName + "@" + module.name

    assert(name.count(_ == '@') == 1)

    override def toString = name

    def apply(ks: K*) = KApply(this, KList(ks.toList))
  }

  case class KApply[KK <: K](klabel: kore.KLabel, klist: kore.KList, att: Att = Att()) extends kore.KApply {
    def items = klist.items
  }

  class KSequence private(val elements: List[K], val att: Att = Att()) extends kore.KSequence {
    val items: java.util.List[K] = elements.asJava
    val kApply: kore.KApply = items.asScala reduceRightOption { (a, b) => KLabelLookup(KLabels.KSEQ)(a, b) } getOrElse {KLabelLookup(KLabels.DOTK)()} match {
      case k: kore.KApply => k
      case x => KLabelLookup(KLabels.KSEQ)(x, KLabelLookup(KLabels.DOTK)())
    }

    def iterator: Iterator[K] = elements.iterator

    override def equals(that: Any) = that match {
      case s: KSequence => s.elements == elements
      case _ => false
    }
  }

  object KSequence {
    def apply(elements: List[K], att: Att = Att()): KSequence =
      new KSequence(elements.foldLeft(List[K]()) {
        case (sum, s: KSequence) => sum ++ s.items.asScala
        case (sum, t) => sum :+ t
      }, att)
  }

  case class KVariable(name: String, att: Att = Att()) extends kore.KVariable {
    def apply(ks: K*) = KApply(this, KList(ks.toList))
  }

  object SortLookup {
    def apply(s: String): SortLookup = SortLookup(s, ModuleName.STAR)
  }

  case class SortLookup(localName: String, moduleName: ModuleName) extends kore.Sort with LookupSymbol {
    override def toString = name

    override def equals(other: Any) = other match {
      case s: Sort if this.moduleName == ModuleName.STAR => s.localName == this.localName
      case s: kore.Sort => this.moduleName == s.moduleName && this.localName == s.localName
      case _ => throw new AssertionError("We cannot compare this.")
    }
  }

  case class Sort(localName: String, moduleName: ModuleName) extends kore.Sort with ResolvedSymbol {
    override def toString = name
    assert(moduleName != ModuleName.STAR)

    override def equals(other: Any) = other match {
      case s: SortLookup if s.moduleName == ModuleName.STAR => this.localName == s.localName
      case s: kore.Sort => s.moduleName == this.moduleName && s.localName == this.localName
      case _ => throw new AssertionError("We cannot compare this.")
    }
  }

  case class KToken(s: String, sort: kore.Sort, att: Att = Att()) extends kore.KToken

  case class KList(elements: List[K]) extends kore.KList {
    elements foreach { e => assert(e.isInstanceOf[K]) }

    def items: java.util.List[K] = elements.asJava

    def iterator: Iterator[K] = elements.iterator
  }

  case class KRewrite(left: kore.K, right: kore.K, att: Att = Att()) extends kore.KRewrite

  case class InjectedKLabel(klabel: kore.KLabel, att: Att) extends kore.InjectedKLabel

}

object SortedADT {

  case class SortedKVariable(name: String, att: Att = Att()) extends kore.KVariable {
    def apply(ks: K*) = ADT.KApply(this, ADT.KList(ks.toList))

    val sort: Sort = ADT.SortLookup(att.getOptional[String]("sort").orElse("K"))

    override def equals(other: Any) = other match {
      case v: SortedKVariable => name == v.name && sort == v.sort
      //      case v: KVariable => throw new UnsupportedOperationException(s"should not mix SortedKVariables with KVariables for variable $this and $v")
      case _ => false
    }
  }

}