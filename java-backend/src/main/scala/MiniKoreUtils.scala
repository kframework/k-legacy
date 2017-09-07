package org.kframework.backend.java

import org.kframework.POSet
import org.kframework.kore._
import org.kframework.utils.errorsystem.KEMException
import org.kframework.minikore.converters.KoreToMini._

import scala.collection.Seq

/**
  * Some utilities needed, for MiniKore to be useful in the Backend.
  */
object MiniKoreUtils {


  def getMainModule(definition: Definition): Module = {
    val mainModuleName = findAtt(definition.att.patterns, iMainModule.str) match {
      case Seq(DomainValue(Symbol("S"), Value(name))) => name;
      case _ => ???
    }

    definition.modules.find(p => p.name.str == mainModuleName).get
  }

  def findAtt(att: Seq[Pattern], key: String): Seq[Pattern] = {
    val argss = att.collect({
      case Application(Symbol(`key`), args) => args
    })
    if (argss.size >= 1)
      argss.head
    else Seq()
  }


  case class ModuleUtils(m: Module, definition: Definition) {


    lazy val importedSentences = getSentencesFromImports()

    lazy val localSentences = m.sentences

    lazy val allSentences: Seq[Sentence] = localSentences ++ importedSentences

    def getSentencesFromImports(): Seq[Sentence] = {
      val mainModuleImports: Set[String] = localSentences collect {
        case Import(ModuleName(name), _) => name
      } toSet

      definition.modules.filter { m: Module => mainModuleImports.contains(m.name.str) } flatMap {
        mod: Module => ModuleUtils(mod, definition).allSentences
      }
    }

    lazy val signatureFor: Map[String, Set[(Seq[Sort], String)]] = {
      allSentences collect {
        case SymbolDeclaration(Sort(sort), Symbol(label), args: Seq[Sort], _)
        => (label, (args, sort))
      } groupBy {_._1} mapValues { x => x map {_._2} toSet }
    }

    lazy val attributesFor: Map[String, Seq[Pattern]] = {
      val filterSet = Set(iTerminal.str, iNonTerminal.str, iRegexTerminal.str)
      val labelDecsMap: Map[String, Seq[Pattern]] =
        allSentences collect {
          // TODO: may need to handle subsort declarations separately
          case SymbolDeclaration(_, Symbol(label), _, Attributes(att)) => (label, att)
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
        case Application(Symbol(label), _) => !filterSet.contains(label)
        case _ => true
      })
    }

    /** Recursively retrieve all defined sorts from the current module, and imported modules.
      *
      */
    lazy val definedSorts: Set[String] = {
      allSentences collect {
        case SymbolDeclaration(Sort(sort), _, _, _) => sort
        case SortDeclaration(Sort(sort), _) => sort
      } toSet
    }

    /**
      * Given a Module m and Definition d, generates a powerset containing all subsorts
      */

    lazy val subsorts: POSet[String] = {
      val symbolDecs: Seq[(String, Seq[Pattern])] = allSentences collect {
        case SymbolDeclaration(Sort(sort), _, _, Attributes(atts)) if findAtt(atts, "klabel").isEmpty => (sort, atts)
      }
      val subsortProductions: Set[(String, String)] = symbolDecs map { x =>
        (x._1, x._2 collect {
          case Application(`iNonTerminal`, Seq(DomainValue(Symbol("S"), Value(s)))) => s
          case Application(`iTerminal`, _) => iTerminal.str
          case Application(`iRegexTerminal`, _) => iTerminal.str
        })
      } filter (x => x._2.size == 1 && !x._2.head.startsWith(iTerminal.str)) map { x => (x._2.head, x._1) } toSet

      POSet[String](subsortProductions)
    }

    lazy val freshFunctionFor: Map[String, String] = {
      val productions = allSentences collect {
        case SymbolDeclaration(sort, Symbol(label), _, Attributes(atts)) if findAtt(atts, "freshGenerator").size >= 1
        => (sort, label)
      } groupBy (_._1.str) mapValues (x => x.toSet)

      productions.foreach(x => {
        if (x._2.size > 1)
          throw KEMException.compilerError("Found more than one fresh generator for sort "
            + x._1 + ". Found" + x._2.map(y => y._2))
      })

      productions.map(x => (x._1, x._2.head._2))
    }

    lazy val symbolDecs: Seq[SymbolDeclaration] = {
      allSentences collect {
        case s@SymbolDeclaration(_, _, _, _) => s
      }
    }

    lazy val rules: Set[Rule] = {
      allSentences collect {
        case x@Rule(_, _) => x
      } toSet
    }

  }

  def decodePatternAttributes(p: Pattern): (Pattern, Seq[Pattern]) = {
    p match {
      case Application(Symbol("#"), Seq(pat, att)) => decodePatternAttributes(pat) match {
        case (finalPat, attList) => (finalPat, attList :+ att)
      }
      case any@_ => (any, Seq())
    }
  }

  def getOriginalModuleMap(d: Definition): Map[String, Module] = {
    d.modules.groupBy(m => m.name.str)
      .mapValues({ case Seq(m) => m; case _ => ??? }) // shouldn't have duplicate module names
  }

}

