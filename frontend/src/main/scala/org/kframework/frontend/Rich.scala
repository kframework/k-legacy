package org.kframework.frontend

import org.kframework.attributes.Att
import org.kframework.definition.Module
import collection.JavaConverters._

case class Rich(theModule: Module) {

  private val module = theModule

  implicit class RichK(k: K) {
    def contains(f: PartialFunction[K, Boolean]) = find(f) != None

    def find(f : K => Boolean): Option[K] = {
      if (f(k))
        Some(k)
      else
        (k match {
          case k: KCollection => k.items.asScala.toStream flatMap (_.find(f)) headOption
          case _ => None
        })
    }

    def find(f: PartialFunction[K, Boolean]): Option[K] = {
      val ff: (K) => Boolean = f.applyOrElse[K, Boolean](_, { k:K => false })
      find(ff)
    }

  }

  implicit class RichKApply(k: KApply) {
    def att = k.klabel.att
  }

  implicit class RichKLabel(klabel: KLabel) {
    def productions = module.productionsFor(klabel)

    def att: Att = Att(productions.flatMap(_.att.att))
  }

}
