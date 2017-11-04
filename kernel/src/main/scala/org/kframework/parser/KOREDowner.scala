package org.kframework.parser

import org.kframework.minikore.KDefinitionDSL._
import org.kframework.minikore.KoreToMini._

import org.apache.commons.lang3.StringEscapeUtils

import org.kframework.minikore.MiniKore._

object MiniKoreStaging {

  // Map
  // ===
  def onChildren(f: Pattern => Pattern): Pattern => Pattern = {
    case Application(label, args) => Application(label, args map f)
    case And(p, q)                => And(f(p), f(q))
    case Or(p, q)                 => Or(f(p), f(q))
    case Not(p)                   => Not(f(p))
    case Implies(p, q)            => Implies(f(p), f(q))
    case Exists(v, p)             => Exists(v, f(p))
    case ForAll(v, p)             => ForAll(v, f(p))
    case Next(p)                  => Next(f(p))
    case Rewrite(p, q)            => Rewrite(f(p), f(q))
    case Equal(p, q)              => Equal(f(p), f(q))
    case p                        => p
  }

  // Traversals
  // ==========

  // `traverseTopDown` will first apply `f` to the root, then apply it to the sub-terms.
  // This will perform better than `traverseBottomUp` when `f: Pattern => Pattern` may eliminate sub-terms.
  def traverseTopDown(f: Pattern => Pattern): Pattern => Pattern = pattern => onChildren(traverseTopDown(f))(f(pattern))

  // `traverseBottomUp` will first apply `f` to the sub-terms, then to the root.
  def traverseBottomUp(f: Pattern => Pattern): Pattern => Pattern = pattern => f(onChildren(traverseBottomUp(f))(pattern))

  // Cons Lists
  // ==========
  // Create cons-lists given the klabel for the `cons` operator and the `nil` operator.
  // consListLeft("apply", "0")(Seq("1","2","3")) => apply(apply(apply(0,1),2),3)
  // consListRight("apply", "4")(Seq("1","2","3")) => apply(1,apply(2,apply(3,4)))

  def consListLeft(cons: String, nil: String)(ps: Seq[Pattern]): Pattern = ps.foldLeft(Application(nil, Seq.empty))((acc, next) => Application(cons, Seq(acc, next)))
  def consListRight(cons: String, nil: String)(ps: Seq[Pattern]): Pattern = ps.foldRight(Application(nil, Seq.empty))((acc, next) => Application(cons, Seq(next, acc)))

  // Flatten parse-trees
  // ===================
  // flattenByLabels(apply(apply(apply(0,1),2),3)) => Seq("0","1","2","3")
  // flattenByLabels(apply(1,apply(2,apply(3,4)))) => Seq("1","2","3","4")

  def flattenByLabels(labels: String*): Pattern => Seq[Pattern] = {
    case Application(label, args) if labels contains label => args flatMap flattenByLabels(labels:_*)
    case parsed                                            => Seq(parsed)
  }
}

object MiniKoreMeta {
  import MiniKoreStaging._

  // TODO: I would like to make the downers take things of type Application (instead of Pattern), but that
  // means that all of the recursively downed parts of the matched patterns have to be type annotated, which
  // is quite verbose and ugly. Should we make a new subsort of just Application in a trait called "MetaPattern"
  // or something like that?

  def makeDomainValueByName(name: String)(concrete: String): DomainValue = DomainValue(name, concrete)
  def getDomainValueByName(name: String): DomainValue => String = { case DomainValue(`name`, value) => value }

  def makeSymbol(concrete: String) : DomainValue = makeDomainValueByName("KSymbol@KTOKENS")(concrete)
  def getSymbol(parsed: Pattern): String = getDomainValueByName("KSymbol@KTOKENS")(downDomainValue(parsed))

  val upDomainValue: DomainValue => Application = { case DomainValue(name, value) => Application("KMLDomainValue", Seq(Application(name, Nil), Application(value, Nil))) }
  val downDomainValue: Pattern => DomainValue = {
    case Application("KMLDomainValue", Application(name, Nil) :: Application(value, Nil) :: Nil) => DomainValue(name, value)
    case Application(value, Nil) => DomainValue("KSymbol@KTOKENS", value) // TODO: remove/fix this case
  }

  val upVariable: Variable => Application = { case Variable(name, sort) => Application("KMLVariable", Seq(makeSymbol(name), symbol(sort))) }
  val downVariable: Pattern => Variable = { case Application("KMLVariable", DomainValue("KSymbol@KTOKENS", name) :: DomainValue("KSymbol@KTOKENS", sort) :: Nil) => Variable(name, sort) }

  val upPattern: Pattern => Application = {
    case Application(label, args) => Application("KMLApplication", Seq(Application(label, Seq.empty), upPatternList(args)))
    case And(p, q)                => Application("KMLAnd", Seq(upPattern(p), upPattern(q)))
    case Or(p, q)                 => Application("KMLOr",  Seq(upPattern(p), upPattern(q)))
    case Not(p)                   => Application("KMLNot",  Seq(upPattern(p)))
    case Implies(p, q)            => Application("KMLImplies",  Seq(upPattern(p), upPattern(q)))
    case Exists(v, p)             => Application("KMLExists",  Seq(upPattern(p)))
    case ForAll(v, p)             => Application("KMLForAll",  Seq(upPattern(p)))
    case Next(p)                  => Application("KMLNext",  Seq(upPattern(p)))
    case Rewrite(p, q)            => Application("KMLRewrite",  Seq(upPattern(p), upPattern(q)))
    case Equal(p, q)              => Application("KMLEqual",  Seq(upPattern(p), upPattern(q)))
    case vb@Variable(_, _)        => upVariable(vb)
    case dv@DomainValue(_, _)     => upDomainValue(dv)
  }

  val downPattern: Pattern => Pattern = {
    case Application("KMLApplication", label :: pList :: Nil) => Application(getSymbol(label), downPatternList(pList))
    case Application("KMLTrue", Nil)                          => True()
    case Application("KMLFalse", Nil)                         => False()
    case Application("KMLAnd", p1 :: p2 :: Nil)               => And(downPattern(p1), downPattern(p2))
    case Application("KMLOr", p1 :: p2 :: Nil)                => Or(downPattern(p1), downPattern(p2))
    case Application("KMLNot", p :: Nil)                      => Not(downPattern(p))
    case Application("KMLImplies", p1 :: p2 :: Nil)           => Implies(downPattern(p1), downPattern(p2))
    case Application("KMLExists", v :: p :: Nil)              => Exists(downVariable(v), downPattern(p))
    case Application("KMLForall", v :: p :: Nil)              => ForAll(downVariable(v), downPattern(p))
    case Application("KMLNext", p :: Nil)                     => Next(downPattern(p))
    case Application("KMLRewrite", p1 :: p2 :: Nil)           => Rewrite(downPattern(p1), downPattern(p2))
    case Application("KMLEqual", p1 :: p2 :: Nil)             => Equal(downPattern(p1), downPattern(p2))
    case vb@Application("KMLVariable", _)                     => downVariable(vb)
    case dv@Application("KMLDomainValue", _)                  => downDomainValue(dv)
  }

  def upPatternList(concretes: Seq[Pattern]): Pattern = consListRight("KMLPatternList", ".KMLPatternList")(concretes map upPattern)
  def downPatternList(parsed: Pattern): Seq[Pattern] = flattenByLabels("KMLPatternList", ".KMLPatternList")(parsed) map downPattern

  val upAttributes: Attributes => Pattern = {
    case Nil          => Application(".KAttributes", Seq.empty)
    case concreteAtts => Application("KAttributes", Seq(upPatternList(concreteAtts)))
  }
  def downAttributes(parsed: Pattern): Attributes = flattenByLabels("KAttributes", ".KAttributes")(parsed) flatMap downPatternList

  def upSymbolList(concrete: Seq[String]): Pattern = upPatternList(concrete map (cs => makeSymbol(cs)))
  def getSymbolList(parsed: Pattern): Seq[String] = downPatternList(parsed) map { case DomainValue("KSymbol@KTOKENS", value) => value }

  val upSentence: Sentence => Pattern = _ => ???
  def downSentence(parsed: Pattern): Sentence = parsed match {
    case Application("KImport", importName :: atts :: Nil)        => Import(getSymbol(importName), downAttributes(atts))
    case Application("KSortDeclaration", sortName :: atts :: Nil) => SortDeclaration(getSymbol(sortName), downAttributes(atts))
    case Application("KSymbolDeclaration", sortName :: Application("KMLApplication", label :: args :: Nil) :: atts :: Nil)
                                                                  => SymbolDeclaration(getSymbol(sortName), getSymbol(label), getSymbolList(args), downAttributes(atts))
    case Application("KSymbolDeclaration", sortName :: Application(regex, Nil) :: atts :: Nil)
                                                                  => SymbolDeclaration(getSymbol(sortName), regex, Seq.empty, downAttributes(atts))
    case Application("KRule", rule :: atts :: Nil)                => dummySentence(Application(iBubble, Seq(S("rule"), S(getSymbol(rule).replaceAll("\\s+$", "").replaceAll("^\\s+^", "")))) +: downAttributes(atts))
  }

  val upModule: Module => Pattern = {
    case Module(name: String, sentences: Seq[Sentence], atts: Attributes) => Application("KModule", Seq(makeSymbol(name), consListRight("KSentenceList", ".KSentenceList")(sentences map upSentence), upAttributes(atts)))
  }
  val downModule: Pattern => Module = {
    case Application("KModule", name :: sentences :: atts :: Nil) => Module(getSymbol(name), flattenByLabels("KSentenceList", ".KSentenceList")(sentences) map downSentence, downAttributes(atts))
  }

  val upDefinition: Definition => Pattern = {
     case Definition(modules: Seq[Module], atts: Attributes) => Application("KDefinition", Seq(upAttributes(atts), consListRight("KModuleList", ".KModuleList")(modules map upModule)))
  }
  val downDefinition: Pattern => Definition = {
     case Application("KDefinition", atts :: modules :: Nil) => Definition(flattenByLabels("KModuleList", ".KModuleList")(modules) map downModule, downAttributes(atts))
  }
 
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

object MetaPasses {
  import MiniKoreStaging._
  import MiniKoreMeta._

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
}
