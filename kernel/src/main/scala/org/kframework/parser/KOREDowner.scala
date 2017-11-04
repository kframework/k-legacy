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

object MetaPasses {

  // TODO: move to minikore
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
  
  def flattenHashes(parsed: Pattern): Pattern = parsed match {
    case Application("#", Application(label, args) :: rest) => Application(label, args ++ rest)
    case _                                                  => parsed
  }

  def removeNodesByLabel(label: String)(parsed: Pattern): Pattern = parsed match {
    case Application(l, children) => Application(l, children filterNot { case Application(`label`, _) => true case _ => false })
    case _                        => parsed
  }

  def unescapeStrings(parsed: Pattern): Pattern = parsed match {
    // TODO: return KString or KString@TOKENS?
    case DomainValue("KString@KTOKENS", value) => DomainValue("KString", StringEscapeUtils.unescapeJava(value.drop(1).dropRight(1)))
    case _                             => parsed
  }

  def flattenToken(parsed: Pattern): Pattern = parsed match {
    case Application("#",List(DomainValue(sort, value))) => DomainValue(sort, value)
    case _                                               => parsed
  }

  def removeParseInfo(parsed: Pattern): Pattern = removeNodesByLabel("org.kframework.attributes.Source")(removeNodesByLabel("org.kframework.attributes.Location")(flattenHashes(parsed)))

  def allPasses(parsed: Pattern): Pattern = flattenToken(unescapeStrings(removeParseInfo(parsed)))
  def preProcess(parsed: Pattern): Pattern = traverse(allPasses)(parsed)

}

object KOREDowner {
  
  def downVariable(parsedVariable: Pattern): Variable = parsedVariable match {
    case Application("KMLVariable", DomainValue("KSymbol@KTOKENS", name) :: DomainValue("KSymbol@KTOKENS", sort) :: _) => Variable(name, sort)
  }

  def downPatternList(parsedPatternList: Pattern): Seq[Pattern] = parsedPatternList match {
    case Application(".KPatternList", _)                    => Seq.empty
    case Application("KPatternList", pattern :: pList :: _) => downPattern(pattern) +: downPatternList(pList)

    case dv@DomainValue(_, _)                               => Seq(dv) // TODO: figure out what we want here
    case _                                                  => Seq(downPattern(parsedPatternList))
  }

  def downPattern(parsedPattern: Pattern): Pattern = parsedPattern match {
    case Application("KMLApplication", DomainValue("KSymbol@KTOKENS", label) :: pList :: _)                   => Application(label, downPatternList(pList))
    case Application("KMLValue", DomainValue("KSymbol@KTOKENS", label) :: DomainValue("KString", value) :: _) => DomainValue(label, value)
    case Application("KMLTrue", _)                                                                            => True()
    case Application("KMLFalse", _)                                                                           => False()
    case Application("KMLAnd", p1 :: p2 :: _)                                                                 => And(downPattern(p1), downPattern(p2))
    case Application("KMLOr", p1 :: p2 :: _)                                                                  => Or(downPattern(p1), downPattern(p2))
    case Application("KMLNot", p :: _)                                                                        => Not(downPattern(p))
    case Application("KMLImplies", p1 :: p2 :: _)                                                             => Implies(downPattern(p1), downPattern(p2))
    case Application("KMLExists", v :: p :: _)                                                                => Exists(downVariable(v), downPattern(p))
    case Application("KMLForall", v :: p :: _)                                                                => ForAll(downVariable(v), downPattern(p))
    case Application("KMLNext", p :: _)                                                                       => Next(downPattern(p))
    case Application("KMLRewrite", p1 :: p2 :: _)                                                             => Rewrite(downPattern(p1), downPattern(p2))
    case Application("KMLEqual", p1 :: p2 :: _)                                                               => Equal(downPattern(p1), downPattern(p2))
    case _                                                                                                    => downVariable(parsedPattern)
  }



  def downAttributes(parsedAttributes: Pattern): Attributes = parsedAttributes match {
    case Application(".KAttributes", _)        => Seq.empty
    case Application("KAttributes", atts :: _) => downPatternList(atts)
    case _                                     => Seq(downPattern(parsedAttributes))
  }

  def downSymbolList(symbolList: Pattern): Seq[String] = symbolList match {
    case DomainValue("KSymbol@KTOKENS", label) => Seq(label)
    case Application(".KMLPatternList", _) => Seq.empty
    case Application("KMLPatternList", args) => args flatMap downSymbolList
  }

  def downCtor(parsedCtor: Pattern): (String, Seq[String]) = parsedCtor match {
    case DomainValue("KSymbol@KTOKENS", label) => (label, Seq.empty)
    case Application("KMLApplication", DomainValue("KSymbol@KTOKENS", label) :: args :: _) => (label, downSymbolList(args))
  }

  def flattenProduction(parsedProduction: Pattern): Seq[Pattern] = parsedProduction match {
    case Application("KProduction", p1 :: p2 :: _) => flattenProduction(p1) ++ flattenProduction(p2)
    case _                                         => Seq(parsedProduction)
  }

  def downSyntaxSentences(parsedSentence: Pattern): Seq[Sentence] = parsedSentence match {
    case Application("KSentenceList", sentences)                                                       => sentences flatMap ((sent: Pattern) => downSyntaxSentences(sent))
    case Application("KSortDeclaration", DomainValue("KSymbol@KTOKENS", sortName) :: atts :: _)                => Seq(SortDeclaration(sortName, downAttributes(atts)))
    case Application("KSymbolDeclaration", DomainValue("KSymbol@KTOKENS", sortName) :: ctor :: atts :: _)      => Seq(SymbolDeclaration(sortName, downCtor(ctor)._1, downCtor(ctor)._2, downAttributes(atts)))

    // TODO: Move this case to a pre-processing extension (instead of in kore)
    case Application("KSyntaxProduction", DomainValue("KSymbol@KTOKENS", sortName) :: production :: atts :: _) => {
      val productionItems = flattenProduction(production) // map downProductionItem
      Seq(SymbolDeclaration(sortName, getKLabel(downAttributes(atts)).getOrElse(makeKLabel(productionItems)), productionItems collect { case Application(`iNonTerminal`, Seq(DomainValue("S", s))) => s }, downAttributes(atts) :+ kprod(productionItems)))
    }

    case Application("KImport", DomainValue("KSymbol@KTOKENS", importName) :: atts :: _)                       => Seq(Import(importName, downAttributes(atts)))
    case Application("KRule", DomainValue("KBubble", rule) :: atts :: _)                                       => Seq(dummySentence(Application(iBubble, Seq(S("rule"), S(rule.replaceAll("\\s+$", "").replaceAll("^\\s+^", "")))) +: downAttributes(atts)))
    case _                                                                                                     => Seq.empty
  }

  def downModules(parsedModules: Pattern): Seq[Module] = parsedModules match {
    case Application("KModuleList", module :: modules :: _)                                 => downModules(modules) ++ downModules(module)
    case Application("KModule", DomainValue("KSymbol@KTOKENS", name) :: sentences :: atts :: _) => Seq(Module(name, downSyntaxSentences(sentences), downAttributes(atts)))
    case _                                                                                  => Seq.empty
 }

 // TODO: Make this chase the requires list
 def downDefinition(parsedDefinition: Pattern): Definition = parsedDefinition match {
    case Application("KDefinition", atts :: modules :: _) => Definition(downModules(modules), downAttributes(atts))
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
