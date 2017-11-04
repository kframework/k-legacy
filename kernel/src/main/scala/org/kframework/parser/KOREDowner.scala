package org.kframework.parser

import org.kframework.parser.concrete2kore.ParseInModule
import org.kframework.attributes.Source

import org.kframework.definition.KOREDefinition._
import org.kframework.definition.KDefinitionDSL._
//import org.kframework.definition._
//import org.kframework.attributes.Att
import org.kframework.kore.ADT.{KList => Args, _}
import org.kframework.kore.K

import org.apache.commons.lang3.StringEscapeUtils

import org.kframework.minikore.MiniKore._


object KOREDowner {

  def preProcess(parsed: K): K = parsed match {
    case KToken(str, KString, atts)         => KToken(StringEscapeUtils.unescapeJava(str.drop(1).dropRight(1)), KString, atts)
    case kt@KToken(_, _, _)                 => kt
    case KApply(head, Args(subNodes), atts) => KApply(head, Args(subNodes map preProcess), atts)
  }

  def downKKeyList(parsedKKeyList: K): List[String] = parsedKKeyList match {
    case KApply(KLabelLookup("KKeyList"), Args(kkeys), _) => kkeys flatMap downKKeyList
    case KToken(att, KAttributeKey, _)                    => List(att)
    case _                                                => List.empty
  }

  def downKKeySet(parsedKKeySet: K): Set[String] = parsedKKeySet match {
    case KApply(KLabelLookup("KKeySet"), Args(kkeys), _) => kkeys.toSet flatMap downKKeySet
    case KToken(att, KAttributeKey, _)                   => Set(att)
    case _                                               => Set.empty
  }
  
  def downVariable(parsedVariable: K): Variable = parsedVariable match {
    case KApply(KLabelLookup("KMLVariable"), Args(KToken(name, KSymbol, _) :: KToken(sort, KSymbol, _) :: _), _) => Variable(name, sort)
  }

  def downPatternList(parsedPatternList: K): Seq[Pattern] = parsedPatternList match {
    case KApply(KLabelLookup(".KPatternList"), _, _)                           => Seq.empty
    case KApply(KLabelLookup("KPatternList"), Args(kPattern :: pList :: _), _) => downPattern(kPattern) +: downPatternList(pList)
    case _                                                                     => Seq(downPattern(parsedPatternList))
  }

  def downPattern(parsedPattern: K): Pattern = parsedPattern match {
    case KApply(KLabelLookup("KMLApplication"), Args(KToken(label, KSymbol, _) :: pList :: _), _)               => Application(label, downPatternList(pList))
    case KApply(KLabelLookup("KMLValue"), Args(KToken(label, KSymbol, _) :: KToken(value, KString, _) :: _), _) => DomainValue(label, value)
    case KApply(KLabelLookup("KMLTrue"), _, _)                                                                  => True()
    case KApply(KLabelLookup("KMLFalse"), _, _)                                                                 => False()
    case KApply(KLabelLookup("KMLAnd"), Args(k1 :: k2 :: _), _) => And(downPattern(k1), downPattern(k2))
    case KApply(KLabelLookup("KMLOr"), Args(k1 :: k2 :: _), _) => Or(downPattern(k1), downPattern(k2))
    case KApply(KLabelLookup("KMLNot"), Args(k :: _), _) => Not(downPattern(k))
    case KApply(KLabelLookup("KMLImplies"), Args(k1 :: k2 :: _), _) => Implies(downPattern(k1), downPattern(k2))
    case KApply(KLabelLookup("KMLExists"), Args(v :: k :: _), _) => Exists(downVariable(v), downPattern(k))
    case KApply(KLabelLookup("KMLForall"), Args(v :: k :: _), _) => ForAll(downVariable(v), downPattern(k))
    case KApply(KLabelLookup("KMLNext"), Args(k :: _), _) => Next(downPattern(k))
    case KApply(KLabelLookup("KMLRewrite"), Args(k1 :: k2 :: _), _) => Rewrite(downPattern(k1), downPattern(k2))
    case KApply(KLabelLookup("KMLEqual"), Args(k1 :: k2 :: _), _) => Equal(downPattern(k1), downPattern(k2))
    case _ => downVariable(parsedPattern)
  }

  def downAttributes(parsedAttributes: K): Attributes = parsedAttributes match {
    case KApply(KLabelLookup("KAttributes"), Args(atts), _)                                              => atts map downPattern
//    case KApply(KLabelLookup("KAttributeApply"), Args(KToken(fnc, KAttributeKey, _) :: keyList :: _), _) => Att(asKApply(fnc, downKKeyList(keyList)))
//    case KToken(attName, KAttributeKey, _)                                                               => Att(attName)
  }

//  def downProduction(parsedProduction: K): Seq[ProductionItem] = parsedProduction match {
//    case KApply(KLabelLookup("KProductionItems"), Args(productionItems), _)    => productionItems flatMap downProduction
//    case KApply(KLabelLookup("KRegex"), Args(KToken(str, KString, _) :: _), _) => Seq(RegexTerminal("#", str, "#"))
//    case KToken(sortName, KSort, _)                                            => Seq(NonTerminal(sortName))
//    case KToken(str, KString, _)                                               => Seq(Terminal(str))
//    case _                                                                     => Seq.empty
//  }

//  def downPriorityBlocks(parsedPriority: K): Seq[Set[Tag]] = parsedPriority match {
//    case KApply(KLabelLookup("KPriorityItems"), Args(priorityBlocks), _) => priorityBlocks flatMap downPriorityBlocks
//    case _                                                               => Seq(downKKeySet(parsedPriority) map Tag)
//  }

  def downSortList(parsed)

  def downSyntaxSentences(parsedSentence: K): Seq[Sentence] = parsedSentence match {
    case KApply(KLabelLookup("KSentenceList"), Args(sentences), _)                                                 => sentences flatMap ((sent: K) => downSyntaxSentences(sent, Seq.empty))
    //case KApply(KLabelLookup("KSentenceWithAttributes"), Args(sentence :: newAtts :: _), _)                => downSyntaxSentences(sentence, downAttributes(newAtts) ++ atts)
    case KApply(KLabelLookup("KSortDeclaration"), Args(KToken(sortName, KSort, _) :: atts :: _), _)                => Seq(SortDeclaration(sortName, downAttributes(atts)))
    case KApply(KLabelLookup("KSyntaxProduction"), Args(KToken(sortName, KSort, _) :: production :: atts :: _), _) => ??? //Seq(SymbolDeclaration(sortName, getKLabel(atts), downProduction(production), atts)
    case KApply(KLabelLookup("KSymbolDeclaration"), Args(KToken(sortName, KSort, _) :: KToken(label, KSymbol, _) :: sortList :: atts :: _), _) => Seq(SymbolDeclaration(sortName, label, downSortList(sortList), downAttributes(atts)))
    // case KApply(KLabelLookup("KSyntaxPriority"), Args(priority :: _), _)                                   => Seq(SyntaxPriority(downPriorityBlocks(priority), atts))
    case KApply(KLabelLookup("KRule"), Args(KToken(rule, KBubble, _) :: _), _)                             => Seq(Bubble("rule", rule.replaceAll("\\s+$", "").replaceAll("^\\s+^", ""), atts))
    case _                                                                                                 => Seq.empty
  }

  def downImports(parsedImports: K): Seq[Sentence] = parsedImports match {
    case KApply(KLabelLookup("KImportList"), Args(importStmt :: rest :: _), _)               => downImports(importStmt) ++ downImports(rest)
    case KApply(KLabelLookup("KImport"), Args(KToken(importModule, KModuleName, _) :: _), _) => Seq(Import(importModule, Seq.empty))
    case _                                                                                   => Seq.empty
  }

  // TODO: Make this chase the requires list
  def downModules(parsedModule: K, downedModules: Map[String, Module]): Map[String, Module] = parsedModule match {
    case KApply(KLabelLookup("KDefinition"), Args(requires :: modules :: _), _)                              => downModules(modules, downModules(requires, downedModules))
    case KApply(KLabelLookup("KRequireList"), _, _)                                                          => downedModules
    case KApply(KLabelLookup("KModuleList"), Args(module :: modules :: _), _)                                => downModules(modules, downModules(module, downedModules))
    case KApply(KLabelLookup("KModule"), Args(KToken(name, KModuleName, _) :: imports :: sentences :: _), _) => downedModules ++ Map(name -> Module(name, downImports(imports) map downedModules toSet, downSyntaxSentences(sentences)))
    case _                                                                                                   => downedModules
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
