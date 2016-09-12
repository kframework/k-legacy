package org.kframework.definition

import collection._
import JavaConverters._

object ModuleName {
  val STAR = ModuleName("*")
}

case class ModuleName(s: String) {
  override def toString = s
}

trait ModuleQualified {
  val localName: String
  val moduleName: ModuleName
  def name: String = localName + (if (moduleName != ModuleName.STAR) "@" + moduleName else "")
  override def hashCode = localName.hashCode
}

trait LookupSymbol extends ModuleQualified

trait ResolvedSymbol extends ModuleQualified

case class SymbolResolver[L <: ModuleQualified, S <: ResolvedSymbol](val moduleName: String, imported: Set[SymbolResolver[L, S]], definedLookups: Set[L])
                                                                    (implicit makeL: (String, ModuleName) => L, makeS: (String, ModuleName) => S)
  extends (L => S) {

  val thisNamespace = ModuleName(moduleName)

  def makeTooManySymbolsErrorMessage(lookup: L, lookups: Set[S]): String = {
    "While defining module " + this.thisNamespace + ": "
    "Found too many symbols named: " + lookup + ". Possible symbols: " + lookups.mkString(", ")
  }

  private def starify(moduleName: ModuleName) = if (moduleName == thisNamespace) ModuleName.STAR else moduleName
  private def localizeStar(moduleName: ModuleName) = if (moduleName == ModuleName.STAR) thisNamespace else moduleName

  private def tryToDefine(l: L): Option[S] =
    lookupInImported(l) match {
      case None => Some(makeS(l.localName, localizeStar(l.moduleName)))
      case Some(s) => None
    }

  def lookupInImported(ll: L): Option[S] = {
    val l = makeL(ll.localName, starify(ll.moduleName))

    val importedSymbols: Set[S] = imported flatMap { sr => sr.get(l) }

    if (importedSymbols.size > 1) {
      throw new RuntimeException(makeTooManySymbolsErrorMessage(l, importedSymbols))
    }

    importedSymbols.headOption
  }

  val defined: Set[S] = definedLookups flatMap tryToDefine

  val memoizedGet = new java.util.concurrent.ConcurrentHashMap[L, Option[S]]().asScala

  // do not replace with getOrElseUpdate as this way avoids a
  // Scala compiler bug which eliminates the call-by-name memoization when it shouldn't
  def get(l: L): Option[S] = memoizedGet.getOrElseUpdate(l, {
    defined
      .find(s => s.localName == l.localName && (s.moduleName == l.moduleName || starify(l.moduleName) == ModuleName.STAR))
      // TODO: remove "|| starify(l.moduleName) == ModuleName.STAR)" when frontend steps are cleaner
      .orElse(lookupInImported(l))
  })

  def apply(l: L): S = get(l).getOrElse(
    throw new AssertionError("While defining module " + this.thisNamespace + ": Could not find symbol " + l))
}
