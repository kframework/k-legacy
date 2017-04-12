package org.kframework.kore

import org.kframework.builtin.{KLabels, Sorts}
import org.kframework.kore
import org.kframework.attributes._
import org.kframework.utils.errorsystem.KEMException

import collection.JavaConverters._
import org.kframework.definition._

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
    def size = klist.size
    def asIterable = klist.asIterable
  }

  class KSequence private(val elements: List[K], val att: Att = Att()) extends kore.KSequence {
    val items: java.util.List[K] = elements.asJava
    val size: Int = elements.size
    val asIterable: java.lang.Iterable[K] = new org.kframework.List(elements)
    lazy val kApply: kore.KApply = items.asScala reduceRightOption { (a, b) => KLabelLookup(KLabels.KSEQ)(a, b) } getOrElse {KLabelLookup(KLabels.DOTK)()} match {
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
    private val emptyAtt = Att()

    def raw(elements: scala.collection.immutable.List[K]): KSequence =
      new KSequence(elements, emptyAtt)

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
    def apply(s: String): SortLookup = {
      s.count(_ == '@') match {
        case 0 => SortLookup(s, ModuleName.STAR)
        case 1 =>
          val ssplit = s.split("@")
          SortLookup(ssplit(0), ModuleName(ssplit(1)))
        case 2 => throw new AssertionError("Sort name contains multiple @s")
      }
    }
  }

  case class SortLookup(localName: String, moduleName: ModuleName) extends kore.Sort with LookupSymbol {
    override def toString = name

    override def equals(other: Any) = other match {
      case s: Sort if this.moduleName == ModuleName.STAR => s.localName == this.localName
      case s: kore.Sort => this.moduleName == s.moduleName && this.localName == s.localName
      case _ => throw new AssertionError("We cannot compare this.")
    }

    override def name = super[LookupSymbol].name // hack for compiler bug
  }

  case class Sort(localName: String, moduleName: ModuleName) extends kore.Sort with ResolvedSymbol {
    override def toString = name
    assert(moduleName != ModuleName.STAR)

    override def equals(other: Any) = other match {
      case s: SortLookup if s.moduleName == ModuleName.STAR => this.localName == s.localName
      case s: kore.Sort => s.moduleName == this.moduleName && s.localName == this.localName
      case _ => throw new AssertionError("We cannot compare " + other + " to " + this)
    }

    override def name = super[ResolvedSymbol].name // hack for compiler bug
  }

  case class KToken(s: String, sort: kore.Sort, att: Att = Att()) extends kore.KToken

  case class KList(elements: List[K]) extends kore.KList {
    elements foreach { e => assert(e.isInstanceOf[K]) }

    def items: java.util.List[K] = elements.asJava

    def iterator: Iterator[K] = elements.iterator
    lazy val size = elements.size
    lazy val asIterable = new org.kframework.List(elements)
  }

  case class KRewrite(left: kore.K, right: kore.K, att: Att = Att()) extends kore.KRewrite

  case class InjectedKLabel(klabel: kore.KLabel, att: Att) extends kore.InjectedKLabel

}

object SortedADT {

  case class SortedKVariable(name: String, att: Att = Att()) extends kore.KVariable {
    def apply(ks: K*) = ADT.KApply(this, ADT.KList(ks.toList))

    val sort: Sort = att.getOptional[Sort](Att.sort).orElse(Sorts.K)

    override def equals(other: Any) = other match {
      case v: SortedKVariable => name == v.name && sort == v.sort
      //      case v: KVariable => throw new UnsupportedOperationException(s"should not mix SortedKVariables with KVariables for variable $this and $v")
      case _ => false
    }
  }

}