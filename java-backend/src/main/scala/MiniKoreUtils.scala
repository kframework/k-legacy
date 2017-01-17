package org.kframework.backend.java
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

  def findAtt(att: Attributes, key: String): Seq[Pattern] = {
    val argss = att.collect({
      case Application(`key`, args) => args
    })
    assert(argss.size == 1)
    argss.head
  }

  //TODO: signature for method need
  def signatureFor(m: Module): Map[String, Set[(Seq[SortDeclaration], SortDeclaration)]] = ???

  def attributesFor(m: Module): Map[String, Seq[Pattern]] = ???




}

