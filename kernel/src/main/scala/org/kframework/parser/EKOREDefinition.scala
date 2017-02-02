package org.kframework.parser

import org.apache.commons.lang3.StringEscapeUtils
import org.kframework.minikore.MiniKore._
import org.kframework.minikore.KoreToMini._
import org.kframework.minikore.MiniKorePatternUtils._
import org.kframework.minikore.MiniKoreMeta._
import org.kframework.parser.KDefinitionDSL._


object ParserNormalization {

  // Utilities
  // =========

  def stripString(front: Int, back: Int): String => String = (str: String) => StringEscapeUtils.unescapeJava(str drop front dropRight back)

  // Normalization passes
  // ====================

  val removeParseInfo: Pattern => Pattern = {
    case Application("#", Application("#", actual :: _) :: _) => actual
    case pattern => pattern
  }

  val normalizeTokens: Pattern => Pattern = {
    case dv@DomainValue("KSymbol@KTOKENS", _) => upDomainValue(dv)
    case DomainValue(name@"KString@KTOKENS", str) => upDomainValue(DomainValue(name, stripString(1, 1)(str)))
    case DomainValue("KMLPattern@KML", name) => Application("KMLApplication", Seq(upSymbol(name)))
    case pattern => pattern
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

  val preProcess: Pattern => Pattern = traverseTopDown(removeParseInfo) andThen traverseBottomUp(normalizeTokens)
}

object EKOREDefinition {
  import ParserNormalization._
  import KOREDefinition._

  // K-PRETTY-PRODUCTION
  // ===================

  val KTerminal      = Sort("KTerminal")
  val KRegexTerminal = Sort("KRegexTerminal")
  val KNonTerminal   = Sort("KNonTerminal")
  val KProduction    = Sort("KProduction")
  val KProductions   = Sort("KProductions")
  val KPriority      = Sort("KPriority")

  val K_PRETTY_PRODUCTION: Module = module("K-PRETTY-PRODUCTION",
    imports("KDEFINITION"),

    syntax(KTerminal)      is Regex(KRegexString) att "token",
    syntax(KRegexTerminal) is Regex("r" + KRegexString) att "token",
    syntax(KNonTerminal)   is Regex(KRegexSymbol) att "token",

    syntax(KProduction) is KTerminal,
    syntax(KProduction) is KRegexTerminal,
    syntax(KProduction) is KNonTerminal,
    syntax(KProduction) is (KProduction, KProduction) att(klabel("KProduction"), "assoc"),

    syntax(KProductions) is (KProduction, KAttributes) att klabel("KProductionWithAttributes"),
    syntax(KProductions) is (KProduction, KAttributes, "|", KProductions) att klabel("KProductions"),

    syntax(KPriority) is KMLPatternList,
    syntax(KPriority) is (KPriority, ">", KPriority) att(klabel("KPriorityItems"), "assoc"),

    syntax(KSentence) is ("syntax", KSymbol, KAttributes) att klabel("KSortDeclaration"),
    syntax(KSentence) is ("syntax", KSymbol, "::=", KProductions) att klabel("KSyntaxProduction"),
    syntax(KSentence) is ("syntax", "priority", KPriority, KAttributes) att klabel("KSyntaxPriority")
  )

  // TODO: correctly process precede/follow regex clauses
  val normalizeProductionItems: Pattern => Pattern = {
    case DomainValue(name@"KTerminal@K-PRETTY-PRODUCTION", term)       => application(name, stripString(1, 1)(term))
    case DomainValue(name@"KRegexTerminal@K-PRETTY-PRODUCTION", rterm) => Application(name, Seq(Application("#", Nil), Application(stripString(2, 1)(rterm), Nil), Application("#", Nil)))
    case DomainValue(name@"KNonTerminal@K-PRETTY-PRODUCTION", nterm)   => application(name, nterm)
    case pattern                                                       => pattern
  }

  val desugarSyntaxProduction: Pattern => Pattern = {
    case Application("KSyntaxProduction", sort :: Application("KProductionWithAttributes", production :: atts :: Nil) :: Nil) => {
      val downedAtts = downAttributes(atts)
      val prodItems  = flattenByLabels("KProduction")(traverseBottomUp(normalizeProductionItems)(production))
      val newKLabel  = upSymbol(getKLabel(downedAtts).getOrElse(prodItems map makeCtorString mkString))
      val args       = prodItems collect { case Application("KNonTerminal@K-PRETTY-PRODUCTION", Application(nt, Nil) :: Nil) => upSymbol(nt) }
      Application("KSymbolDeclaration", Seq(sort, newKLabel, consListLeft("KSymbolList", ".KSymbolList")(args), upAttributes(downedAtts :+ prod(prodItems))))
    }
    case Application("KSyntaxProduction", sort :: Application("KProductions", production :: atts :: productions :: Nil) :: Nil) => {
      val firstSent = Application("KSyntaxProduction", Seq(sort, Application("KProductionWithAttributes", Seq(production, atts))))
      val restSent  = Application("KSyntaxProduction", Seq(sort, productions))
      Application("KSentenceList", Seq(firstSent, restSent))
    }
    case pattern => pattern
  }

  // K-CONCRETE-RULES
  // ================

  val KBubbleRegex = "[^ \n\r\t]+"

  val KBubbleItem = Sort("KBubbleItem")
  val KBubble = Sort("KBubble")

  val K_CONCRETE_RULES: Module = module("K-CONCRETE-RULES",
    imports("KML"),

    syntax(KBubbleItem) is Regex(KBubbleRegex) att("token", application("reject2", "rule|syntax|endmodule|configuration|context")),

    syntax(KBubble) is (KBubble, KBubbleItem) att(klabel("KBubble"), "avoid", "token"),
    syntax(KBubble) is KBubbleItem att("avoid", "token"),

    syntax(KMLPattern) is KBubble
  )

  def mkParser(d: Definition): String => Pattern = input => {
    import org.kframework.parser.concrete2kore.ParseInModule
    import org.kframework.attributes.Source
    import org.kframework.kore.ADT.SortLookup
    import org.kframework.minikore.MiniToKore
    import org.kframework.minikore.KoreToMini
    import org.kframework.minikore.MiniKoreOuterUtils._

    val mainModuleName = getAttributeKey(`KoreToMini.iMainModule`, d.att) match { Seq(Seq(Application(name, Nil))) => name }
    val allSorts = allSorts(d)
    val newModules = d.modules collect {
      case Module(`mainModuleName`, sentences, att) => {
        val newSentences = allSorts(d) flatMap (sort => Seq(syntax(sort) is KMLVariable, syntax(KMLPattern) is sort))
        Module(mainModuleName, sentences ++ newSentences, att)
      }
      case module => module
    }
    val ruleParserDef = Definition(newModules, d.att)

    val parser = new ParseInModule(MiniToKore(onAttributesDef(traverseTopDown(toKoreEncoding))(ruleParserDef)))
    parser.parseString(input, SortLookup("KMLPattern"), Source(""))._1 match {
      case Right(x) => KoreToMini(x)
      case Left(y) => throw new Error("runParser error: " + y.toString)
    }
  }

  def resolveRuleBubbles(parser: String => Pattern): Pattern => Pattern = {
    case b@Application("KBubble", _) => parser(flattenByLabels("KBubble")(b) map { case DomainValue("KBubbleItem@K-CONCRETE-RULES", str) => str } mkString)
    case pattern                     => pattern
  }

  // EKORE
  // =====

  val EKORE_MODULE: Module = module("EKORE",
    imports("K-PRETTY-PRODUCTION"),
    imports("K-CONCRETE-RULES")
  )

  val EKORE = definition((KORE.modules :+ K_PRETTY_PRODUCTION :+ K_CONCRETE_RULES :+ EKORE_MODULE):_*) att(application(iMainModule, "EKORE"), application(iEntryModules, "EKORE"))

  val ekoreToKore: Pattern => Pattern = traverseTopDown(desugarSyntaxProduction)
}

