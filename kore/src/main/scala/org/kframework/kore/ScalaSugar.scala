package org.kframework.kore

import org.kframework.attributes.Att
import org.kframework.builtin.Sorts
import org.kframework.kore

import scala.collection.JavaConverters._

class ScalaSugar[K <: kore.K](cons: Constructors[K]) {

  import cons._

  implicit def stringToToken(s: String) = KToken(Sorts.String, s, Att())
  def stringToId(s: String): K = KToken(Sorts.Id, s, Att())
  implicit def symbolToLabel(l: Symbol): KLabel = module.KLabel(l.name)
  implicit def intToToken(n: Int): K = KToken(Sorts.Int, n.toString, Att())

  implicit class ApplicableKLabel(klabel: KLabel) {
    def apply(l: K*): K = KApply(klabel, KList(l), Att())
  }

  implicit class ApplicableSymbol(klabel: Symbol) {
    def apply(l: K*): K = KApply(klabel: KLabel, KList(l), Att())
  }

  implicit class EnhancedK(k: K) {
    def ~>(other: kore.K) = KSequence(Seq(k, other).asJava, Att())
    def ==>(other: K) = KRewrite(k, other, Att())
    def +(other: K) = cons.KLabel("+")(k, other)
    def -(other: K) = cons.KLabel("-")(k, other)
    def *(other: K) = cons.KLabel("*")(k, other)
    def /(other: K): K = cons.KLabel("/")(k, other)
    def ~(other: K): K = cons.KLabel("~")(k, other)
    def &&(other: K) = cons.KLabel("&&")(k, other)
    def ||(other: K) = cons.KLabel("||")(k, other)
  }

  def KList[KK <: K](ks: Seq[KK]): KList = cons.KList(ks.asJava)
  //  def KApply[KK <: K](klabel: KLabel, ks: Seq[KK], att: Att = Att()): K = cons.KApply(klabel, KList(ks), att)
}
