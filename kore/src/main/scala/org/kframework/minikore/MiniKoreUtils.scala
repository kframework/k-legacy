package org.kframework.minikore

import org.kframework.minikore.KoreToMini.iMainModule
import org.kframework.minikore.MiniKore._

import scala.collection.Seq

/**
  * Some utilities needed, for MiniKore to be useful in the Backend.
  */
object MiniKoreUtils {

  def getMainModule(definition: Definition) : Module  = {
    val mainModuleName = findAtt(definition.att, iMainModule) match {
      case Seq(DomainValue("S", name)) => name; case _ => ???
    }

    definition.modules.find(p => p.name == mainModuleName).get
  }

  //Todo: Move from MiniToKore to Utils
  def findAtt(att: Attributes, key: String): Seq[Pattern] = {
    val argss = att.collect({
      case Application(`key`, args) => args
    })
    assert(argss.size == 1)
    argss.head
  }
}
