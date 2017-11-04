package org.kframework.parser

import org.apache.commons.lang3.StringEscapeUtils

import org.kframework.minikore.MiniKore._
import org.kframework.minikore.KoreToMini._
import org.kframework.minikore.MiniKorePatternUtils._
import org.kframework.minikore.MiniKoreMeta._

import org.kframework.parser.KDefinitionDSL._


object EKOREDefinition {
  import org.kframework.parser.KOREDefinition._

  def stripString(front: Int, back: Int): String => String = (str: String) => StringEscapeUtils.unescapeJava(str drop front dropRight back)

  // K-PRETTY-PRODUCTION
  // ===================

  val KTerminal      = Sort("KTerminal")
  val KRegexTerminal = Sort("KRegexTerminal")
  val KNonTerminal   = Sort("KNonTerminal")
  val KProduction    = Sort("KProduction")

  val K_PRETTY_PRODUCTION: Module = module("K-PRETTY-PRODUCTION",
    imports("KDEFINITION"),

    syntax(KTerminal)      is Regex(KRegexString) att "token",
    syntax(KRegexTerminal) is Regex("r" + KRegexString) att "token",
    syntax(KNonTerminal)   is Regex(KRegexSymbol) att "token",

    syntax(KProduction) is KTerminal,
    syntax(KProduction) is KRegexTerminal,
    syntax(KProduction) is KNonTerminal,
    syntax(KProduction) is (KProduction, KProduction) att(klabel("KProduction"), "assoc"),

    syntax(KSentence) is ("syntax", KSymbol, KAttributes) att klabel("KSortDeclaration"),
    syntax(KSentence) is ("syntax", KSymbol, "::=", KProduction, KAttributes) att klabel("KSyntaxProduction")
  )

  val normalizeProductions: Pattern => Pattern = {
    case DomainValue(name@"KTerminal@K-PRETTY-PRODUCTION", term)       => application(name, stripString(1, 1)(term))
    case DomainValue(name@"KRegexTerminal@K-PRETTY-PRODUCTION", rterm) => application(name, stripString(2, 1)(rterm))
    case DomainValue(name@"KNonTerminal@K-PRETTY-PRODUCTION", nterm)   => application(name, nterm)
    case pattern                                                       => pattern
  }

  val syntaxProductionToSymbolDeclaration: Pattern => Pattern = {
    case Application("KSyntaxProduction", sortName :: production :: atts :: Nil) => {
      val downedAtts = downAttributes(atts)
      val prodItems  = flattenByLabels("KProduction")(production)
      val newKLabel  = upDomainValue(makeSymbol(getKLabel(downedAtts).getOrElse(prodItems map makeCtorString mkString)))
      val args       = prodItems collect { case Application("KNonTerminal@K-PRETTY-PRODUCTION", Application(nt, Nil) :: Nil) => upSymbol(nt) }
      Application("KSymbolDeclaration", Seq(sortName, newKLabel, consListLeft("KSymbolList", ".KSymbolList")(args), upAttributes(downedAtts :+ prod(prodItems))))
    }
    case pattern => pattern
  }

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

  // EKORE
  // =====

  val EKORE = definition((KORE.modules :+ K_PRETTY_PRODUCTION):_*) att(application(iMainModule, "K-PRETTY-PRODUCTION"), application(iEntryModules, "K-PRETTY-PRODUCTION"))

  val ekoreToKore: Pattern => Pattern = traverseBottomUp(normalizeProductions andThen syntaxProductionToSymbolDeclaration)
}

object KToMiniKorePasses {
  import EKOREDefinition._

  // Normalization passes
  // ====================

  val removeParseInfo: Pattern => Pattern = {
    case Application("#", Application("#", actual :: _) :: _) => actual
    case parsed                                               => parsed
  }

  val normalizeTokens: Pattern => Pattern = {
    case dv@DomainValue("KSymbol@KTOKENS", _)     => upDomainValue(dv)
    case DomainValue(name@"KString@KTOKENS", str) => upDomainValue(DomainValue(name, stripString(1,1)(str)))
    case DomainValue("KMLPattern@KML", name)      => application("KMLApplication", name)
    case parsed                                   => parsed
  }

  // Disagreements on encoding
  // =========================

  def toKoreEncoding: Pattern => Pattern = {
    case Application("KTerminal@K-PRETTY-PRODUCTION", Application(str, Nil) :: followRegex) => Application(iTerminal, S(str) :: followRegex)
    case Application("KRegexTerminal@K-PRETTY-PRODUCTION", Application(precede, Nil) :: Application(regex, Nil) :: Application(follow, Nil) :: Nil)
                                                                                            => Application(iRegexTerminal, Seq(S(precede), S(regex), S(follow)))
    case Application("KNonTerminal@K-PRETTY-PRODUCTION", Application(str, Nil) :: Nil)      => Application(iNonTerminal, Seq(S(str)))
    case Application(`iMainModule`, Application(modName, Nil) :: Nil)                       => Application(iMainModule, Seq(S(modName)))
    case Application(`iEntryModules`, Application(modName, Nil) :: Nil)                     => Application(iEntryModules, Seq(S(modName)))
    case pattern                                                                            => pattern
  }

  // Preprocessing
  // =============
  // `preProcess` first prunes the parse-tree using a top-down traversal of `removeParseInfo`
  // then normalizes the defintion by running a bottom-up traversal of `syntaxProductionToSymbolDeclaration . normalizeMetaDomainValues . unescapeStrings`

  val preProcess: Pattern => Pattern =
    traverseTopDown(removeParseInfo) andThen
    traverseBottomUp(normalizeTokens) andThen
    ekoreToKore
}
