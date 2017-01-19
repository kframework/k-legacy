package org.kframework.backend.java

import org.kframework.minikore.KoreToMini.iMainModule
import org.kframework.minikore.MiniKore._

import scala.collection.{Seq, mutable}

/**
  * Some utilities needed, for MiniKore to be useful in the Backend.
  */
object MiniKoreUtils {

  def getMainModule(definition: Definition): Module = {
    val mainModuleName = findAtt(definition.att, iMainModule) match {
      case Seq(DomainValue("S", name)) => name;
      case _ => ???
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


  /**
    * Given a module m and a definition, return all sentences from modules imported (recursively) by m.
    */
  def allSentences(m: Module, definition: Definition): Seq[Sentence] = {
    val mainModuleImports: Set[String] = m.sentences collect {
      case Import(name, _) => name
    } toSet

    val importedSentences: Seq[Sentence] = definition.modules.filter(p => mainModuleImports.contains(p.name))
      .flatMap(x => allSentences(x, definition))

    m.sentences ++ importedSentences
  }

  def signatureFor(m: Module, definition: Definition): Map[String, Set[(Seq[String], String)]] = {
    allSentences(m, definition) collect {
      case SymbolDeclaration(sort: String, label: String, args: Seq[String], _)
      => (label, Set((args, sort)))
    } filter (p => !(p._1.isEmpty)) toMap
  }

  def attributesFor(m: Module, definition: Definition): Map[String, Seq[Pattern]] = {
    val atts = allSentences(m, definition) collect {
      case SymbolDeclaration(_, label: String, _, att: Attributes) => (label, att)
    } filter (p => !p._1.isEmpty)
    atts.groupBy(_._1).mapValues(x => x.flatMap(_._2))
  }

  def freshFunctionFor(m: Module): Map[String, String] = {
    val map: Map[String, String] = Map.empty
    m.sentences.foreach(x => x match {
      case SymbolDeclaration(sort, label, _, att) => {
        if (findAtt(att, "freshGenerator").size == 1) {
          map + (sort -> label)
        }
      }
      case _ => None
    })
    map
  }

  def definedSorts(m: Module): Set[SortDeclaration] = ???

  //  def productions(m: Module): Set[_] = {
  //    m.sentences match {
  //      case SymbolDeclaration(sort, _, _, att) =>
  //
  //    }
  //  }

  def rules(m: Module): Seq[Rule] = ???

  def localRules(m: Module): Seq[Rule] = ???


}

