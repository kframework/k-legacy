package org.kframework.parser

import org.kframework.parser.concrete2kore.ParseInModule
import org.kframework.attributes.Source

import org.kframework.definition.KOREDefinition._
import org.kframework.definition.KDefinitionDSL._
import org.kframework.definition._
import org.kframework.attributes.Att
import org.kframework.kore.ADT.{KList => Args, _}
import org.kframework.kore._

import org.apache.commons.lang3.StringEscapeUtils


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

  def downAttributes(parsedAttributes: K): Att = parsedAttributes match {
    case KApply(KLabelLookup("KAttributes"), Args(atts), _)                                              => atts.foldLeft(Att()) ((accAtt: Att, newAtt: K) => accAtt ++ downAttributes(newAtt))
    case KApply(KLabelLookup("KAttributeApply"), Args(KToken(fnc, KAttributeKey, _) :: keyList :: _), _) => Att(asKApply(fnc, downKKeyList(keyList)))
    case KToken(attName, KAttributeKey, _)                                                               => Att(attName)
    case _                                                                                               => Att()
  }

  def downProduction(parsedProduction: K): Seq[ProductionItem] = parsedProduction match {
    case KApply(KLabelLookup("KProductionItems"), Args(productionItems), _)    => productionItems flatMap downProduction
    case KApply(KLabelLookup("KRegex"), Args(KToken(str, KString, _) :: _), _) => Seq(RegexTerminal("#", str, "#"))
    case KToken(sortName, KSort, _)                                            => Seq(NonTerminal(sortName))
    case KToken(str, KString, _)                                               => Seq(Terminal(str))
    case _                                                                     => Seq.empty
  }

  def downPriorityBlocks(parsedPriority: K): Seq[Set[Tag]] = parsedPriority match {
    case KApply(KLabelLookup("KPriorityItems"), Args(priorityBlocks), _) => priorityBlocks flatMap downPriorityBlocks
    case _                                                               => Seq(downKKeySet(parsedPriority) map Tag)
  }

  def downSyntaxSentences(parsedSentence: K, atts: Att = Att()): Set[Sentence] = parsedSentence match {
    case KApply(KLabelLookup("KSentenceList"), Args(sentences), _)                                         => sentences.toSet flatMap ((pS: K) => downSyntaxSentences(pS, Att()))
    case KApply(KLabelLookup("KSentenceWithAttributes"), Args(sentence :: newAtts :: _), _)                => downSyntaxSentences(sentence, downAttributes(newAtts) ++ atts)
    case KApply(KLabelLookup("KSyntaxSort"), Args(KToken(sortName, KSort, _) :: _), _)                     => Set(SyntaxSort(SortLookup(sortName), atts))
    case KApply(KLabelLookup("KSyntaxProduction"), Args(KToken(sortName, KSort, _) :: production :: _), _) => Set(Production(SortLookup(sortName), downProduction(production), atts))
    case KApply(KLabelLookup("KSyntaxPriority"), Args(priority :: _), _)                                   => Set(SyntaxPriority(downPriorityBlocks(priority), atts))
    case KApply(KLabelLookup("KRule"), Args(KToken(rule, KBubble, _) :: _), _)                             => Set(Bubble("rule", rule.replaceAll("\\s+$", "").replaceAll("^\\s+^", ""), atts))
    case _                                                                                                 => Set.empty
  }

  def downImports(parsedImports: K): List[String] = parsedImports match {
    case KApply(KLabelLookup("KImportList"), Args(importStmt :: rest :: _), _)               => downImports(importStmt) ++ downImports(rest)
    case KApply(KLabelLookup("KImport"), Args(KToken(importModule, KModuleName, _) :: _), _) => List(importModule)
    case _                                                                                   => List.empty
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
    val diamondKMLSubsorts = module.localSorts flatMap (sort => Set(Production(sort, Seq(NonTerminal(KMLVar)), Att()), Production(KMLTerm, Seq(NonTerminal(sort)), Att())))
    val parser = new ParseInModule(Module(module.name, module.imports + KML, module.localSentences ++ diamondKMLSubsorts))
    val resolvedRules: Set[Rule] = module.localSentences
        .collect { case Bubble("rule", rule, atts) =>
          parser.parseString(rule, KMLRewrite, Source(""))._1 match {
            case Right(KApply(KLabelLookup("KMLRewrite"), Args(lhs :: rhs :: _), _)) => Rule(KRewrite(lhs, rhs, atts), KORE.KToken("true", ADT.SortLookup("KBool")), KORE.KToken("true", ADT.SortLookup("KBool")))
            case Right(_) => throw new Error("Error: Non-rewrite bubble in rule: " ++ rule)
            case Left(y) => throw new Error("Error parsing rule: " ++ rule ++ "\n" ++ y.toString)
          }
        }.toSet  // TODO: remove this toSet (it turns scala.collection.Set => scala.collection.immutable.Set)
    val moduleSentences = module.localSentences.filter { case x:Bubble => false case _ => true } ++ diamondKMLSubsorts ++ resolvedRules
    Module(module.name, module.imports + KML + KBOOL, moduleSentences.toSet)
  }
}
