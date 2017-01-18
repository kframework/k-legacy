package org.kframework.parser

import org.kframework.parser.concrete2kore.ParseInModule
import org.kframework.attributes.Source
import org.kframework.minikore.KOREDefinition._
import org.kframework.minikore.KDefinitionDSL._
import org.kframework.minikore.KoreToMini._
//import org.kframework.kore.ADT.{KList => Args, _}
//import org.kframework.kore.K

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

  def flattenByLabel(label: String)(parsed: Pattern): Seq[Pattern] = parsed match {
    case Application(`label`, args) => args flatMap flattenByLabel(label)
    case _                          => Seq(parsed)
  }

  def removeNodesByLabel(label: String)(parsed: Pattern): Pattern = parsed match {
    case Application(l, children) => Application(l, children filterNot { case Application(`label`, _) => true case _ => false })
    case _                        => parsed
  }
}

object MiniKoreMeta {
  import MiniKoreStaging._

  // TODO: I would like to make the downers take things of type Application (instead of Pattern), but that
  // means that all of the recursively downed parts of the matched patterns have to be type annotated, which
  // is quite verbose and ugly. Should we make a new subsort of just Application in a trait called "MetaPattern"
  // or something like that?

  def upVariable(concrete: Variable): Application = concrete match {
    case Variable(name, sort) => Application("KMLVariable", Seq(symbol(name), symbol(sort)))
  }

  def downVariable(parsedVariable: Pattern): Variable = parsedVariable match {
    case Application("KMLVariable", DomainValue("KSymbol@KTOKENS", name) :: DomainValue("KSymbol@KTOKENS", sort) :: Nil) => Variable(name, sort)
  }

  // TODO: Should we pass this over the entire definition before doing any downing/processing? To get it into proper Mete-Level SExp form?
  def upDomainValue(concreteDomainValue: DomainValue): Application = concreteDomainValue match {
    case DomainValue(name, value) => Application("KMLDomainValue", Seq(Application(name, Nil), Application(value, Nil)))
  }

  def downDomainValue(parsedDomainValue: Pattern): DomainValue = parsedDomainValue match {
    case Application("KMLDomainValue", Application(name, Nil) :: Application(value, Nil) :: Nil) => DomainValue(name, value)
  }

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

  def downPattern(parsedPattern: Pattern): Pattern = parsedPattern match {
    case Application("KMLApplication", Application(label, Nil) :: (pList@Application(_, _)) :: Nil) => Application(label, downPatternList(pList))
    case Application("KMLTrue", Nil)                                                                => True()
    case Application("KMLFalse", Nil)                                                               => False()
    case Application("KMLAnd", p1 :: p2 :: Nil)                                                     => And(downPattern(p1), downPattern(p2))
    case Application("KMLOr", p1 :: p2 :: Nil)                                                      => Or(downPattern(p1), downPattern(p2))
    case Application("KMLNot", p :: Nil)                                                            => Not(downPattern(p))
    case Application("KMLImplies", p1 :: p2 :: Nil)                                                 => Implies(downPattern(p1), downPattern(p2))
    case Application("KMLExists", v :: p :: Nil)                                                    => Exists(downVariable(v), downPattern(p))
    case Application("KMLForall", v :: p :: Nil)                                                    => ForAll(downVariable(v), downPattern(p))
    case Application("KMLNext", p :: Nil)                                                           => Next(downPattern(p))
    case Application("KMLRewrite", p1 :: p2 :: Nil)                                                 => Rewrite(downPattern(p1), downPattern(p2))
    case Application("KMLEqual", p1 :: p2 :: Nil)                                                   => Equal(downPattern(p1), downPattern(p2))
    case vb@Application("KMLVariable", _)                                                           => downVariable(vb)
    case dv@Application("KMLDomainValue", _)                                                        => downDomainValue(dv)
  }

  // TODO: Replace with fold?
  def upPatternList(concrete: Seq[Pattern]): Pattern = concrete match {
    case Nil        => Application(".KPatternList", Seq.empty)
    case c1 :: rest => Application("KPatternList", Seq(upPattern(c1), upPatternList(rest)))
  }

  def downPatternList(parsedPatternList: Pattern): Seq[Pattern] = parsedPatternList match {
    case Application(".KPatternList", Nil)                    => Seq.empty
    case Application("KPatternList", pattern :: pList :: Nil) => downPattern(pattern) +: downPatternList(pList)
    case _                                                    => Seq(downPattern(parsedPatternList))
  }

  def upAttributes(concreteAttributes: Attributes): Pattern = concreteAttributes match {
    case Nil => Application(".KAttributes", Seq.empty)
    case _   => Application("KAttributes", Seq(upPatternList(concreteAttributes)))
  }

  def downAttributes(parsedAttributes: Pattern): Attributes = parsedAttributes match {
    case Application(".KAttributes", Nil)        => Seq.empty
    case Application("KAttributes", atts :: Nil) => downPatternList(atts)
    case _                                       => Seq(downPattern(parsedAttributes))
  }

  def upSymbolList(concreteSymbolList: Seq[String]): Pattern = upPatternList(concreteSymbolList map (cs => symbol(cs)))

  def downSymbolList(parsedSymbolList: Pattern): Seq[String] = parsedSymbolList match {
    //case DomainValue("KSymbol@KTOKENS", label) => Seq(label)
    case Application(".KMLPatternList", Nil)   => Seq.empty
    case Application(label, Nil)               => Seq(label)
    case Application("KMLPatternList", args)   => args flatMap downSymbolList
  }

  def upSentences(concreteSentence: Seq[Sentence]): Pattern = ???

  def downSyntaxSentences(parsedSentence: Pattern): Seq[Sentence] = parsedSentence match {
    case Application("KSentenceList", sentences)                                                  => sentences flatMap ((sent: Pattern) => downSyntaxSentences(sent))
    case Application("KImport", DomainValue("KSymbol@KTOKENS", importName) :: atts :: Nil)        => Seq(Import(importName, downAttributes(atts)))
    case Application("KSortDeclaration", DomainValue("KSymbol@KTOKENS", sortName) :: atts :: Nil) => Seq(SortDeclaration(sortName, downAttributes(atts)))
    case Application("KRule", DomainValue("KBubble", rule) :: atts :: Nil)                        => Seq(dummySentence(Application(iBubble, Seq(S("rule"), S(rule.replaceAll("\\s+$", "").replaceAll("^\\s+^", "")))) +: downAttributes(atts)))
    case Application("KSymbolDeclaration", DomainValue("KSymbol@KTOKENS", sortName) :: Application("KMLApplication", DomainValue("KSymbol@KTokens", label) :: args :: Nil) :: atts :: Nil)
                                                                                                  => Seq(SymbolDeclaration(sortName, label, downSymbolList(args), downAttributes(atts)))
  }

  def upModule(concreteModule: Module): Pattern = ???

  def downModules(parsedModules: Pattern): Seq[Module] = parsedModules match {
    case Application(".KModuleList", Nil)                                           => Seq.empty
    case Application("KModuleList", module :: modules :: Nil)                       => downModules(modules) ++ downModules(module)
    case Application("KModule", Application(name, Nil) :: sentences :: atts :: Nil) => Seq(Module(name, downSyntaxSentences(sentences), downAttributes(atts)))
 }

 def upDefinition(concreteDefinition: Definition): Pattern = ???

 // TODO: Make this chase the requires list
  def downDefinition(parsedDefinition: Pattern): Definition = parsedDefinition match {
     case Application("KDefinition", atts :: modules :: Nil) => Definition(downModules(modules), downAttributes(atts))
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

  def flattenHashes(parsed: Pattern): Pattern = parsed match {
    case Application("#", Application(label, args) :: rest) => Application(label, args ++ rest)
    case Application("#", (dv@DomainValue(_, _)) :: Nil)    => dv
    case _                                                  => parsed
  }

  def unescapeStrings(parsed: Pattern): Pattern = parsed match {
    case DomainValue("KString@KTOKENS", value) => DomainValue("KString@KTOKENS", StringEscapeUtils.unescapeJava(value.drop(1).dropRight(1)))
    case _                                     => parsed
  }

  def syntaxProductionToSymbolDeclaration(parsed: Pattern): Pattern = parsed match {
    case Application("KSyntaxProduction", (sn@DomainValue("KSymbol@KTOKENS", _)) :: production :: Application("KAttributes", atts :: Nil) :: Nil) => {
      val productionItems = flattenByLabel("KProduction")(production)
      val downedAtts      = downAttributes(atts)
      val symbolKLabel    = getKLabel(downedAtts).getOrElse(makeCtorString(productionItems))
      val ctor            = Application(symbolKLabel, productionItems collect { case nt@Application(`iNonTerminal`, Seq(DomainValue("S", s))) => nt })
      Application("KSymbolDeclaration", Seq(sn, ctor, upAttributes(downedAtts :+ kprod(productionItems))))
    }
    case _ => parsed
  }

  def removeParseInfo(parsed: Pattern): Pattern = removeNodesByLabel("org.kframework.attributes.Source")(removeNodesByLabel("org.kframework.attributes.Location")(flattenHashes(parsed)))

  def desugarEKOREToKORE(parsed: Pattern): Pattern = syntaxProductionToSymbolDeclaration(parsed)

  def allPasses(parsed: Pattern): Pattern = desugarEKOREToKORE(unescapeStrings(removeParseInfo(parsed)))

  def preProcess(parsed: Pattern): Pattern = traverse(allPasses)(parsed)
}
