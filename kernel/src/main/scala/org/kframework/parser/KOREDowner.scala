package org.kframework.parser

import org.kframework.parser.concrete2kore.ParseInModule
import org.kframework.attributes.Source
import org.kframework.minikore.KOREDefinition._
import org.kframework.minikore.KDefinitionDSL._
import org.kframework.minikore.KoreToMini._

import org.apache.commons.lang3.StringEscapeUtils

import org.kframework.minikore.MiniKore._

object MiniKoreStaging {
  // NB: no alpha checking done, cannot use for substitution, etc.
  def traverse(f: Pattern => Pattern)(parsed: Pattern): Pattern = parsed match {
    case Application(label, args) => f(Application(label, args map traverse(f)))
    case And(p, q)                => f(And(traverse(f)(p), traverse(f)(q)))
    case Or(p, q)                 => f(Or(traverse(f)(p), traverse(f)(q)))
    case Not(p)                   => f(Not(traverse(f)(p)))
    case Implies(p, q)            => f(Implies(traverse(f)(p), traverse(f)(q)))
    case Exists(v, p)             => f(Exists(v, traverse(f)(p)))
    case ForAll(v, p)             => f(ForAll(v, traverse(f)(p)))
    case Next(p)                  => f(Next(traverse(f)(p)))
    case Rewrite(p, q)            => f(Rewrite(traverse(f)(p), traverse(f)(q)))
    case Equal(p, q)              => f(Equal(traverse(f)(p), traverse(f)(q)))
    case _                        => f(parsed)
  }

  def consListLeft(cons: String, nil: String)(ps: Seq[Pattern]): Pattern = ps.foldLeft(Application(nil, Seq.empty))((acc, next) => Application(cons, Seq(acc, next)))
  def consListRight(cons: String, nil: String)(ps: Seq[Pattern]): Pattern = ps.foldRight(Application(nil, Seq.empty))((acc, next) => Application(cons, Seq(acc, next)))

  def flattenByLabels(labels: String*)(parsed: Pattern): Seq[Pattern] = parsed match {
    case Application(label, args) if labels contains label => args flatMap flattenByLabels(labels:_*)
    case _                                                 => Seq(parsed)
  }

}

object MiniKoreMeta {
  import MiniKoreStaging._

  // TODO: I would like to make the downers take things of type Application (instead of Pattern), but that
  // means that all of the recursively downed parts of the matched patterns have to be type annotated, which
  // is quite verbose and ugly. Should we make a new subsort of just Application in a trait called "MetaPattern"
  // or something like that?

  def makeDomainValueByName(name: String)(concrete: String): DomainValue = DomainValue(name, concrete)
  def getDomainValueByName(name: String)(dv: DomainValue): String = dv match { case DomainValue(`name`, value) => value }

  def getSymbol(parsed: Pattern): String = getDomainValueByName("KSymbol@KTOKENS")(downDomainValue(parsed))
  def makeSymbol(concrete: String) : DomainValue = makeDomainValueByName("KSymbol@KTOKENS")(concrete)

  def upDomainValue(concrete: DomainValue): Application = concrete match { case DomainValue(name, value) => Application("KMLDomainValue", Seq(Application(name, Nil), Application(value, Nil))) }
  def downDomainValue(parsed: Pattern): DomainValue = parsed match {
    case Application("KMLDomainValue", Application(name, Nil) :: Application(value, Nil) :: Nil) => DomainValue(name, value)
    case Application(value, Nil) => DomainValue("KSymbol@KTOKENS", value) // TODO: remove/fix this case
  }

  def upVariable(concrete: Variable): Application = concrete match { case Variable(name, sort) => Application("KMLVariable", Seq(makeSymbol(name), symbol(sort))) }
  def downVariable(parsed: Pattern): Variable = parsed match { case Application("KMLVariable", DomainValue("KSymbol@KTOKENS", name) :: DomainValue("KSymbol@KTOKENS", sort) :: Nil) => Variable(name, sort) }

  def upPattern(concretePattern: Pattern): Application = concretePattern match {
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

  def downPattern(parsed: Pattern): Pattern = parsed match {
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

  def upAttributes(concreteAttributes: Attributes): Pattern = concreteAttributes match {
    case Nil => Application(".KAttributes", Seq.empty)
    case _   => Application("KAttributes", Seq(upPatternList(concreteAttributes)))
  }
  def downAttributes(parsed: Pattern): Attributes = flattenByLabels("KAttributes", ".KAttributes")(parsed) flatMap downPatternList

  def upSymbolList(concrete: Seq[String]): Pattern = upPatternList(concrete map (cs => makeSymbol(cs)))
  def getSymbolList(parsed: Pattern): Seq[String] = downPatternList(parsed) map getSymbol

  def upSentence(concrete: Sentence): Pattern = ???
  def downSentence(parsed: Pattern): Sentence = parsed match {
    case Application("KImport", importName :: atts :: Nil)        => Import(getSymbol(importName), downAttributes(atts))
    case Application("KSortDeclaration", sortName :: atts :: Nil) => SortDeclaration(getSymbol(sortName), downAttributes(atts))
    case Application("KSymbolDeclaration", sortName :: Application("KMLApplication", label :: args :: Nil) :: atts :: Nil)
                                                                  => SymbolDeclaration(getSymbol(sortName), getSymbol(label), getSymbolList(args), downAttributes(atts))
    case Application("KSymbolDeclaration", sortName :: Application(regex, Nil) :: atts :: Nil)
                                                                  => SymbolDeclaration(getSymbol(sortName), regex, Seq.empty, downAttributes(atts))
    case Application("KRule", rule :: atts :: Nil)                => dummySentence(Application(iBubble, Seq(S("rule"), S(getSymbol(rule).replaceAll("\\s+$", "").replaceAll("^\\s+^", "")))) +: downAttributes(atts))
  }

  def upModule(concrete: Module): Pattern = ???
  def downModule(parsed: Pattern): Module = parsed match {
    case Application("KModule", name :: sentences :: atts :: Nil) => Module(getSymbol(name), flattenByLabels("KSentenceList", ".KSentenceList")(sentences) map downSentence, downAttributes(atts))
  }

  // TODO: Make this chase the requires list
  def upDefinition(concrete: Definition): Pattern = ???
  def downDefinition(parsed: Pattern): Definition = parsed match {
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

  def removeParseInfo(parsed: Pattern): Pattern = parsed match {
    case Application("#", Application("#", actual :: _) :: _) => actual
    case _                                                    => parsed
  }

  def unescapeStrings(parsed: Pattern): Pattern = parsed match {
    case DomainValue("KString@KTOKENS", value) => DomainValue("KString@KTOKENS", StringEscapeUtils.unescapeJava(value.drop(1).dropRight(1)))
    case _                                     => parsed
  }

  def normalizeDomainValuesInMeta(parsed: Pattern): Pattern = parsed match {
    case dv@DomainValue(_, _) => upDomainValue(dv)
    case _                    => parsed
  }

  def generateKLabel(productionItems: Seq[Pattern]): String = productionItems map {
    case Application("KRegexTerminal", regex :: Nil) => getDomainValueByName("KString@KTOKENS")(downDomainValue(regex))
    case dv => downDomainValue(dv) match {
      case DomainValue("KSymbol@KTOKENS", _)     => "_"
      case DomainValue("KString@KTOKENS", value) => value
    }
  } mkString

  def syntaxProductionToSymbolDeclaration(parsed: Pattern): Pattern = parsed match {
    case Application("KSyntaxProduction", sortName :: production :: atts :: Nil) => {
      val productionItems = flattenByLabels("KProduction")(production)
      val downedAtts      = downAttributes(atts)
      val ctor            = Application(getKLabel(downedAtts).getOrElse(generateKLabel(productionItems)), productionItems collect { case nt@Application(`iNonTerminal`, Seq(DomainValue("S", s))) => nt })
      Application("KSymbolDeclaration", Seq(sortName, ctor, upAttributes(downedAtts :+ kprod(productionItems))))
    }
    case _ => parsed
  }

  def desugarEKOREToKORE(parsed: Pattern): Pattern = syntaxProductionToSymbolDeclaration(parsed)
  def allPasses(parsed: Pattern): Pattern = desugarEKOREToKORE(normalizeDomainValuesInMeta(removeParseInfo(unescapeStrings(parsed))))
  def preProcess(parsed: Pattern): Pattern = traverse(allPasses)(parsed)
}
