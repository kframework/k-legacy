package org.kframework.parser

import org.kframework.parser.concrete2kore.ParseInModule
import org.kframework.attributes.Source
import org.kframework.definition.KOREDefinition._
import org.kframework.definition.KDefinitionDSL._
import org.kframework.minikore.KoreToMini
import org.kframework.kore.ADT.{KList => Args, _}
import org.kframework.kore.K

import org.apache.commons.lang3.StringEscapeUtils

import org.kframework.minikore.MiniKore._


object KOREDowner {

  def preProcess(parsedConverted: Pattern): Pattern = parsedConverted match {
    case Application(label, args)      => Application(label, args map preProcess)
    case DomainValue("KString", value) => DomainValue("KString", StringEscapeUtils.unescapeJava(value.drop(1).dropRight(1)))
  }

//  def downKKeyList(parsedKKeyList: Pattern): List[String] = parsedKKeyList match {
//    case KApply(KLabelLookup("KKeyList"), Args(kkeys), _) => kkeys flatMap downKKeyList
//    case DomainValue("KAttributeKey", att)                    => List(att)
//    case _                                                => List.empty
//  }
//
//  def downKKeySet(parsedKKeySet: Pattern): Set[String] = parsedKKeySet match {
//    case KApply(KLabelLookup("KKeySet"), Args(kkeys), _) => kkeys.toSet flatMap downKKeySet
//    case DomainValue("KAttributeKey", att)                   => Set(att)
//    case _                                               => Set.empty
//  }
  
  def downVariable(parsedVariable: Pattern): Variable = parsedVariable match {
    case Application("KMLVariable", DomainValue("KSymbol", name) :: DomainValue("KSymbol", sort) :: _) => Variable(name, sort)
  }

  def downPatternList(parsedPatternList: Pattern): Seq[Pattern] = parsedPatternList match {
    case Application(".KPatternList", _)                    => Seq.empty
    case Application("KPatternList", pattern :: pList :: _) => downPattern(pattern) +: downPatternList(pList)
    case _                                                  => Seq(downPattern(parsedPatternList))
  }

  def downPattern(parsedPattern: Pattern): Pattern = parsedPattern match {
    case Application("KMLApplication", DomainValue("KSymbol", label) :: pList :: _)                   => Application(label, downPatternList(pList))
    case Application("KMLValue", DomainValue("KSymbol", label) :: DomainValue("KString", value) :: _) => DomainValue(label, value)
    case Application("KMLTrue", _)                                                                    => True()
    case Application("KMLFalse", _)                                                                   => False()
    case Application("KMLAnd", p1 :: p2 :: _)                                                         => And(downPattern(p1), downPattern(p2))
    case Application("KMLOr", p1 :: p2 :: _)                                                          => Or(downPattern(p1), downPattern(p2))
    case Application("KMLNot", p :: _)                                                                => Not(downPattern(p))
    case Application("KMLImplies", p1 :: p2 :: _)                                                     => Implies(downPattern(p1), downPattern(p2))
    case Application("KMLExists", v :: p :: _)                                                        => Exists(downVariable(v), downPattern(p))
    case Application("KMLForall", v :: p :: _)                                                        => ForAll(downVariable(v), downPattern(p))
    case Application("KMLNext", p :: _)                                                               => Next(downPattern(p))
    case Application("KMLRewrite", p1 :: p2 :: _)                                                     => Rewrite(downPattern(p1), downPattern(p2))
    case Application("KMLEqual", p1 :: p2 :: _)                                                       => Equal(downPattern(p1), downPattern(p2)
    case _                                                                                            => downVariable(parsedPattern)
  }

  def downAttributes(parsedAttributes: Pattern): Attributes = parsedAttributes match {
    case Application(".KAttributes", _)        => Seq.empty
    case Application("KAttributes", atts :: _) => downPatternList(atts)
    case _                                     => Seq(downPattern(parsedAttributes))
  }

  def downSyntaxSentences(parsedSentence: Pattern): Seq[Sentence] = parsedSentence match {
    case Application("KSentenceList", sentences)                                                                                     => sentences flatMap ((sent: Pattern) => downSyntaxSentences(sent, Seq.empty))
    case Application("KSortDeclaration", DomainValue("KSort", sortName) :: atts :: _)                                                => Seq(SortDeclaration(sortName, downAttributes(atts)))
    case Application("KSyntaxProduction", DomainValue("KSort", sortName) :: production :: atts :: _)                                 => ??? //Seq(SymbolDeclaration(sortName, getKLabel(atts), downProduction(production), atts)
    case Application("KSymbolDeclaration", DomainValue("KSort", sortName) :: DomainValue("KSymbol", label) :: sortList :: atts :: _) => Seq(SymbolDeclaration(sortName, label, downSortList(sortList), downAttributes(atts)))
    case Application("KRule", DomainValue("KBubble", rule) :: _)                                                                     => Seq(Bubble("rule", rule.replaceAll("\\s+$", "").replaceAll("^\\s+^", ""), atts))
    case _                                                                                                                           => Seq.empty
    //case KApply(KLabelLookup("KSentenceWithAttributes"), Args(sentence :: newAtts :: _)                => downSyntaxSentences(sentence, downAttributes(newAtts) ++ atts)
    // case KApply(KLabelLookup("KSyntaxPriority"), Args(priority :: _)                                   => Seq(SyntaxPriority(downPriorityBlocks(priority), atts))
  }

  def downImports(parsedImports: Pattern): Seq[Sentence] = parsedImports match {
    case Application("KImportList", importStmt :: rest :: _)                   => downImports(importStmt) ++ downImports(rest)
    case Application("KImport", DomainValue("KModuleName", importModule) :: _) => Seq(Import(importModule, Seq.empty))
    case _                                                                     => Seq.empty
  }

  // TODO: Make this chase the requires list
  def downModules(parsedModule: K, downedModules: Map[String, Module]): Map[String, Module] = parsedModule match {
    case Application("KDefinition", requires :: modules :: _)                                  => downModules(modules, downModules(requires, downedModules))
    case Application("KRequireList",                                                           => downedModules
    case Application("KModuleList", module :: modules :: _)                                    => downModules(modules, downModules(module, downedModules))
    case Application("KModule", DomainValue("KModuleName", name) :: imports :: sentences :: _) => downedModules ++ Map(name -> Module(name, downImports(imports) map downedModules toSet, downSyntaxSentences(sentences)))
    case _                                                                                     => downedModules
  }

  def downRules(module: Module): Module = {
    if (module.name == "KML") return module
    val newImports = (module.imports map downRules) + KML
    val diamondKMLSubsorts = module.localSorts flatMap (sort => Set(Production(sort, Seq(NonTerminal(KMLVar)), Att()), Production(KMLTerm, Seq(NonTerminal(sort)), Att())))
    val parser = new ParseInModule(Module(module.name, newImports, module.localSentences ++ diamondKMLSubsorts))
    val resolvedRules: Set[Rule] = module.localSentences
        .collect { case Bubble("rule", rule, atts) =>
          parser.parseString(rule, KMLRewrite, Source(""))._1 match {
            case Right(KApply(KLabelLookup("KMLRewrite"), Args(lhs :: rhs :: _), _)) => Rule(KRewrite(lhs, rhs, atts), KORE.KToken("tt", ADT.SortLookup("KMLFormula")), KORE.KToken("tt", ADT.SortLookup("KMLFormula")))
            case Right(_) => throw new Error("Error: Non-rewrite bubble in rule: " ++ rule)
            case Left(y) => throw new Error("Error parsing rule: " ++ rule ++ "\n" ++ y.toString)
          }
        }.toSet  // TODO: remove this toSet (it turns scala.collection.Set => scala.collection.immutable.Set)
    val moduleSentences = module.localSentences.filter { case x:Bubble => false case _ => true } ++ diamondKMLSubsorts ++ resolvedRules
    Module(module.name, newImports, moduleSentences.toSet)
  }
}
