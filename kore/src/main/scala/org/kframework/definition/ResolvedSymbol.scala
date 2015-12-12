package org.kframework.definition

object ModuleName {
  val STAR = ModuleName("*")
}

case class ModuleName(s: String)

trait HasLocalName {
  val localName: String
}

trait ModuleQualified {
  val moduleName: ModuleName
}

trait LookupSymbol extends HasLocalName with ModuleQualified

trait ResolvedSymbol extends HasLocalName with ModuleQualified

case class SymbolResolver[L <: LookupSymbol, S <: ResolvedSymbol](val moduleName: String, imported: Set[SymbolResolver[L, S]], definedLookups: Set[L])
                                                                 (implicit makeS: (String, ModuleName) => S, makeL: (String, ModuleName) => L)
  extends (L => Option[S]) {

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

    val importedSymbols: Set[S] = imported flatMap { sr => sr(l) }

    if (importedSymbols.size > 1) {
      throw new RuntimeException(makeTooManySymbolsErrorMessage(l, importedSymbols))
    }

    importedSymbols.headOption
  }

  val defined: Set[S] = definedLookups flatMap tryToDefine

  def apply(l: L): Option[S] = defined
    .find(s => s.localName == l.localName && (s.moduleName == l.moduleName || starify(l.moduleName) == ModuleName.STAR))
    // TODO: remove "|| starify(l.moduleName) == ModuleName.STAR)" when frontend steps are cleaner
    .orElse(lookupInImported(l))
}
