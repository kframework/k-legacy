package org.kframework.parser

import org.kframework.minikore.MiniKore._
import org.kframework.minikore.KoreToMini._
import org.kframework.minikore.MiniKoreOuterUtils._
import org.kframework.minikore.MiniKorePatternUtils._
import org.kframework.minikore.MiniKoreMeta._

import org.apache.commons.lang3.StringEscapeUtils

object KToMiniKorePasses {

  // Normalization passes
  // ====================

  val removeParseInfo: Pattern => Pattern = {
    case Application("#", Application("#", actual :: _) :: _) => actual
    case parsed                                               => parsed
  }

  val unescapeStrings: Pattern => Pattern = {
    case DomainValue("KString@KTOKENS", value) => DomainValue("KString@KTOKENS", StringEscapeUtils.unescapeJava(value.drop(1).dropRight(1)))
    case parsed                                => parsed
  }

  val normalizeMetaDomainValues: Pattern => Pattern = {
    case dv@DomainValue(_, _) => upDomainValue(dv)
    case parsed               => parsed
  }

  // Disagreements on encoding
  // =========================

  def toKoreEncoding: Pattern => Pattern = {
    case Application(KoreToMini.`iNonTerminal`, Application(str, Nil) :: Nil)             => Application(KoreToMini.iNonTerminal, Seq(S(str)))
    case Application(KoreToMini.`iTerminal`, Application(str, Nil) :: followRegex :: Nil) => Application(KoreToMini.iTerminal, Seq(S(str), followRegex))
    case Application(KoreToMini.`iRegexTerminal`, Application(precede, Nil) :: Application(regex, Nil) :: Application(follow, Nil) :: Nil)
                                                                                          => Application(KoreToMini.iRegexTerminal, Seq(S(precede), S(regex), S(follow)))
    case Application(KoreToMini.`iMainModule`, Application(modName) :: Nil)               => Application(KoreToMini.iMainModule, Seq(S(modName)))
    case Application(KoreToMini.`iEntryModules`, Application(modName) :: Nil)             => Application(KoreToMini.iEntryModules, Seq(S(modName)))
    case pattern                                                                          => pattern
  }

  // Extension: KSyntaxProduction => KSymbolDeclaration
  // ==================================================

  def generateKLabel(productionItems: Seq[Pattern]): String = productionItems map {
    case Application("KRegexTerminal", regex :: Nil) => getDomainValueByName("KString@KTOKENS")(downDomainValue(regex))
    case dv => downDomainValue(dv) match {
      case DomainValue("KSymbol@KTOKENS", _)     => "_"
      case DomainValue("KString@KTOKENS", value) => value
    }
  } mkString

  val syntaxProductionToSymbolDeclaration: Pattern => Pattern = {
    case Application("KSyntaxProduction", sortName :: production :: atts :: Nil) => {
      val productionItems = flattenByLabels("KProduction")(production)
      val downedAtts      = downAttributes(atts)
      val ctor            = Application(getKLabel(downedAtts).getOrElse(generateKLabel(productionItems)), productionItems collect { case nt@Application(`iNonTerminal`, Seq(DomainValue("S", s))) => nt })
      Application("KSymbolDeclaration", Seq(sortName, ctor, upAttributes(downedAtts :+ kprod(productionItems))))
    }
    case parsed => parsed
  }

  // Preprocessing
  // =============
  // `preProcess` first prunes the parse-tree using a top-down traversal of `removeParseInfo`
  // then normalizes the defintion by running a bottom-up traversal of `syntaxProductionToSymbolDeclaration . normalizeMetaDomainValues . unescapeStrings`

  val preProcess: Pattern => Pattern =
    traverseTopDown(removeParseInfo) andThen
      traverseBottomUp(unescapeStrings andThen normalizeMetaDomainValues andThen syntaxProductionToSymbolDeclaration)

  // Bubble Resolution
  // =================

  //  def downRules(module: Module): Module = {
  //    if (module.name == "KML") return module
  //    val newImports = (module.imports map downRules) + KML
  //    val diamondKMLSubsorts = module.localSorts flatMap (sort => Set(Production(sort, Seq(NonTerminal(KMLVar)), Att()), Production(KMLTerm, Seq(NonTerminal(sort)), Att())))
  //    val parser = new ParseInModule(Module(module.name, newImports, module.localSentences ++ diamondKMLSubsorts))
  //    val resolvedRules: Set[Rule] = module.localSentences
  //        .collect { case Bubble("rule", rule, atts) =>
  //          parser.parseString(rule, KMLRewrite, Source(""))._1 match {
  //            case Right(KApply(KLabelLookup("KMLRewrite"), Args(lhs :: rhs :: _), _)) => Rule(KRewrite(lhs, rhs, atts), KORE.KToken("tt", ADT.SortLookup("KMLFormula")), KORE.KToken("tt", ADT.SortLookup("KMLFormula")))
  //            case Right(_) => throw new Error("Error: Non-rewrite bubble in rule: " ++ rule)
  //            case Left(y) => throw new Error("Error parsing rule: " ++ rule ++ "\n" ++ y.toString)
  //          }
  //        }.toSet  // TODO: remove this toSet (it turns scala.collection.Set => scala.collection.immutable.Set)
  //    val moduleSentences = module.localSentences.filter { case x:Bubble => false case _ => true } ++ diamondKMLSubsorts ++ resolvedRules
  //    Module(module.name, newImports, moduleSentences.toSet)
  //  }
}
