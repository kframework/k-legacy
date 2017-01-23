package org.kframework.backend.java

import org.kframework.POSet
import org.kframework.minikore.KoreToMini._
import org.kframework.minikore.MiniKore._
import org.kframework.utils.errorsystem.KEMException

import scala.collection.Seq

/**
  * Some utilities needed, for MiniKore to be useful in the Backend.
  */
object MiniKoreUtils {

  var moduleSentenceMap: Map[Module, Seq[Sentence]] = Map()

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
    if (argss.size >= 1)
      argss.head
    else Seq()
  }


  /**
    * Given a module m and a definition, return all sentences from modules imported (recursively) by m.
    */
  def allSentences(m: Module, definition: Definition): Seq[Sentence] = {
    if (moduleSentenceMap.contains(m)) {
      return moduleSentenceMap(m)
    }
    val mainModuleImports: Set[String] = m.sentences collect {
      case Import(name, _) => name
    } toSet

    val importedSentences: Seq[Sentence] = definition.modules.filter(p => mainModuleImports.contains(p.name))
      .flatMap(x => allSentences(x, definition))

    val totalSentences: Seq[Sentence] = m.sentences ++ importedSentences

    moduleSentenceMap + (m -> totalSentences)
    totalSentences
  }


  def signatureFor(m: Module, definition: Definition): Map[String, Set[(Seq[String], String)]] = {
    allSentences(m, definition) collect {
      case SymbolDeclaration(sort: String, label: String, args: Seq[String], _)
      => (label, (args, sort))
    } groupBy {_._1} mapValues { x => x map {_._2} toSet }
  }


  def attributesFor(m: Module, d: Definition): Map[String, Seq[Pattern]] = {
    val filterSet = Set(iTerminal, iNonTerminal, iRegexTerminal)
    val labelDecsMap: Map[String, Seq[Pattern]] =
      allSentences(m, d) collect {
        case SymbolDeclaration(_, label: String, _, att) => (label, att)
      } groupBy { x => x._1 } mapValues { z => z map {_._2} } mapValues {_.flatten}
    labelDecsMap.mapValues({ s =>
      s.flatMap({ p =>
        val decodedTup = decodePatternAttributes(p)
        decodedTup._2 :+ decodedTup._1
      })
    }) mapValues { atts => filterAtts(filterSet, atts) }
  }

  def filterAtts(filterSet: Set[String], atts: Seq[Pattern]): Seq[Pattern] = {
    atts.filter(p => p match {
      case Application(label, _) => !filterSet.contains(label)
      case _ => true
    })
  }


  /** Recursively retrieve all defined sorts from the current module, and imported modules.
    *
    */
  def definedSorts(m: Module, d: Definition): Set[String] = {
    allSentences(m, d) collect {
      case SymbolDeclaration(sort: String, _, _, _) => sort
      case SortDeclaration(sort: String, _) => sort
    } toSet
  }

  /**
    * Given a Module m and Definition d, generates a powerset containing all subsorts
    */

  def subsorts(m: Module, d: Definition): POSet[String] = {
    val symbolDecs: Seq[(String, Seq[Pattern])] = allSentences(m, d) collect {
      case SymbolDeclaration(sort, _, _, atts) => (sort, atts)
    }
    val subsortProductions: Set[(String, String)] = symbolDecs map { x =>
      (x._1, x._2 collect {
        case Application(`iNonTerminal`, Seq(DomainValue("S", s))) => s
        case Application(`iTerminal`, _) => iTerminal
        case Application(`iRegexTerminal`, _) => iTerminal
      })
    } filter (x => x._2.size == 1 && !x._2.head.startsWith(iTerminal)) map { x => (x._2.head, x._1) } toSet

    POSet[String](subsortProductions)
  }

  def freshFunctionFor(m: Module, d: Definition): Map[String, String] = {
    val productions = allSentences(m, d) collect {
      case SymbolDeclaration(sort, label, _, atts) if findAtt(atts, "freshGenerator").size >= 1
      => (sort, label)
    } groupBy (_._1) mapValues (x => x.toSet)

    productions.foreach(x => {
      if (x._2.size > 1) throw KEMException.compilerError("Found more than one fresh generator for sort " + x._1 + ". Found" + x._2.map(y => y._2))
    })

    productions.map(x => (x._1, x._2.head._2))
  }

  def getSymbolDecs(m: Module, d: Definition): Seq[SymbolDeclaration] = {
    allSentences(m, d) collect {
      case s@SymbolDeclaration(_, _, _, _) => s
    }
  }


  def rules(m: Module, d: Definition): Set[Rule] = {
    allSentences(m, d) collect {
      case x@Rule(_, _) => x
    } toSet
  }

  def decodePatternAttributes(p: Pattern): (Pattern, Seq[Pattern]) = {
    p match {
      case Application(`iAtt`, Seq(pat, att)) => decodePatternAttributes(pat) match {
        case (finalPat, attList) => (finalPat, attList :+ att)
      }
      case any@_ => (any, Seq())
    }
  }
}

