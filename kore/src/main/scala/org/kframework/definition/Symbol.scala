package org.kframework.definition

import scala.RuntimeException


case class Namespace(s: String)

trait HasLocalName {
  val localName: String
}

trait HasNamespace {
  val namespace: Namespace
}

trait Lookup extends HasLocalName with HasNamespace

trait Symbol extends HasLocalName with HasNamespace

case class SymbolResolver[L <: Lookup, S <: Symbol](val moduleName: String, imported: Set[SymbolResolver[L, S]], definedLookups: Set[L])
                                              (implicit makeS: (String, Namespace) => S, makeL: (String, Namespace) => L)
  extends (L => Option[S]) {

  val thisNamespace = Namespace(moduleName)

  def makeTooManySymbolsErrorMessage(lookup: L, lookups: Set[S]): String = {
    "While defining module " + this.thisNamespace + ": "
    "Found too many symbols named: " + lookup + ". Possible symbols: " + lookups.mkString(", ")
  }

  private def tryToDefine(l: L): Option[S] =
    lookupInImported(l) match {
      case None => Some(makeS(l.localName, l.namespace))
      case Some(s) => None
    }


  def lookupInImported(l: L): Option[S] = {
    val lookFor: (SymbolResolver[L, S] => Option[S]) =
      if (l.namespace == thisNamespace) {
        sr => sr(makeL(l.localName, sr.thisNamespace))
      } else {
        sr => sr(l)
      }

    val importedSymbols: Set[S] = imported flatMap { sr => lookFor(sr) }

    if (importedSymbols.size > 1) {
      throw new RuntimeException(makeTooManySymbolsErrorMessage(l, importedSymbols))
    }

    importedSymbols.headOption
  }

  val defined: Set[S] = definedLookups flatMap tryToDefine

  def apply(l: L): Option[S] = defined
    .find(s => s.localName == l.localName && (s.namespace == l.namespace || l.namespace == thisNamespace))
    // TODO: remove "|| l.namespace == thisNamespace)" when frontend steps are cleaner
    .orElse(lookupInImported(l))
}
