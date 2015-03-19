package org.kframework.kore

import org.kframework.attributes.Att
import org.kframework.definition.Module
import org.kframework.kore

import scala.collection.JavaConverters._

trait Constructors[K <: kore.K] {
  def module: Module
  def KLabel(name: String, module: Module): KLabel = module.KLabel(name)
  def KLabel(name: String): KLabel = KLabel(name, module)
  def Sort(name: String): Sort
  def KList[KKK <: kore.K](items: java.util.List[KKK]): KList
  def KToken(sort: Sort, s: String, att: Att): K
  def KApply(klabel: KLabel, klist: KList, att: Att): K
  def KSequence[KKK <: kore.K](items: java.util.List[KKK], att: Att): K
  def KVariable(name: String, att: Att): KVariable with K
  def KRewrite(left: kore.K, right: kore.K, att: Att): KRewrite with K
  def InjectedKLabel(klabel: KLabel, att: Att): InjectedKLabel

  val injectedKListLabel = "INJECTED-KLIST"

  // default methods:
  @annotation.varargs def KList(items: kore.K*): KList = KList(items.asJava)
  @annotation.varargs def KApply(klabel: KLabel, items: kore.K*): K = KApply(klabel, KList(items.asJava), Att())
  @annotation.varargs def KSequence(list: kore.K*): K = KSequence(list.toList.asJava, Att())
  @annotation.varargs def KApply(klabelString: String, ks: K*): K = KApply(KLabel(klabelString), KList(ks.asJava), Att())
  def KVariable(name: String): KVariable with K = KVariable(name, Att())
}

abstract class AbstractConstructors[KK <: K] extends Constructors[KK]
