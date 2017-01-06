package org.kframework.parser

import org.kframework.definition.KOREDefinition._
import org.kframework.definition.KDefinitionDSL._
import org.kframework.definition._
import org.kframework.attributes.Att
import org.kframework.kore.ADT.{KList => Args, _}
import org.kframework.kore._

import org.apache.commons.lang3.StringEscapeUtils


object KOREDowner {

  def downKKeyList(parsedKKeyList: K): List[String] = parsedKKeyList match {
    case KApply(KLabelLookup("KKeyList"), Args(kkeys), _) => kkeys flatMap downKKeyList
    case KToken(att, KAttributeKey, _)                     => List(att)
    case _                                                 => List.empty
  }

  def downKKeySet(parsedKKeySet: K): Set[String] = parsedKKeySet match {
    case KApply(KLabelLookup("KKeySet"), Args(kkeys), _) => kkeys.toSet flatMap downKKeySet
    case KToken(att, KAttributeKey, _)                    => Set(att)
    case _                                                => Set.empty
  }

  def downAttributes(parsedAttributes: K): Att = parsedAttributes match {
    case KApply(KLabelLookup("KAttributes"), Args(atts), _)                                              => atts.foldLeft(Att()) ((accAtt: Att, newAtt: K) => accAtt ++ downAttributes(newAtt))
    case KApply(KLabelLookup("KAttributeApply"), Args(KToken(fnc, KAttributeKey, _) :: keyList :: _), _) => Att(asKApply(fnc, downKKeyList(keyList)))
    case KToken(attName, KAttributeKey, _)                                                                => Att(attName)
    case _                                                                                                => Att()
  }

  def downProduction(parsedProduction: K): Seq[ProductionItem] = parsedProduction match {
    case KApply(KLabelLookup("KProductionItems"), Args(productionItems), _)    => productionItems flatMap downProduction
    case KApply(KLabelLookup("KRegex"), Args(KToken(str, KString, _) :: _), _) => Seq(RegexTerminal("#", str, "#"))
    case KToken(sortName, KSort, _)                                             => Seq(NonTerminal(sortName))
    case KToken(str, KString, _)                                                => Seq(Terminal(str))
    case _                                                                      => Seq.empty
  }

  def downPriorityBlocks(parsedPriority: K): Seq[Set[Tag]] = parsedPriority match {
    case KApply(KLabelLookup("KPriorityItems"), Args(priorityBlocks), _) => priorityBlocks flatMap downPriorityBlocks
    case _                                                                => Seq(downKKeySet(parsedPriority) map Tag)
  }

  def downSentences(parsedSentence: K, atts: Att = Att()): Set[Sentence] = parsedSentence match {
    case KApply(KLabelLookup("KSentenceList"), Args(sentences), _)                                   => sentences.toSet flatMap ((pS: K) => downSentences(pS, Att()))
    case KApply(KLabelLookup("KSentenceWithAttributes"), Args(sentence :: newAtts :: _), _)          => downSentences(sentence, downAttributes(newAtts) ++ atts)
    case KApply(KLabelLookup("KSortDecl"), Args(KToken(sortName, KSort, _) :: _), _)                 => Set(SyntaxSort(SortLookup(sortName), atts))
    case KApply(KLabelLookup("KProduction"), Args(KToken(sortName, KSort, _) :: production :: _), _) => Set(Production(SortLookup(sortName), downProduction(production), atts))
    case KApply(KLabelLookup("KPriority"), Args(priority :: _), _)                                   => Set(SyntaxPriority(downPriorityBlocks(priority), atts))
    case _                                                                                            => Set.empty
  }

  def downImports(parsedImports: K): List[String] = parsedImports match {
    case KApply(KLabelLookup("KImportList"), Args(importStmt :: rest :: _), _)               => downImports(importStmt) ++ downImports(rest)
    case KApply(KLabelLookup("KImport"), Args(KToken(importModule, KModuleName, _) :: _), _) => List(importModule)
    case _                                                                                    => List.empty
  }

  // TODO: Make this chase the requires list
  def downModules(parsedModule: K, downedModules: Map[String, Module]): Map[String, Module] = parsedModule match {
    case KApply(KLabelLookup("KDefinition"), Args(requires :: modules :: _), _)                              => downModules(modules, downModules(requires, downedModules))
    case KApply(KLabelLookup("KRequireList"), _, _)                                                           => downedModules
    case KApply(KLabelLookup("KModuleList"), Args(module :: modules :: _), _)                                => downModules(modules, downModules(module, downedModules))
    case KApply(KLabelLookup("KModule"), Args(KToken(name, KModuleName, _) :: imports :: sentences :: _), _) => downedModules ++ Map(name -> Module(name, downImports(imports) map downedModules toSet, downSentences(sentences)))
    case _                                                                                                    => downedModules
  }

  def preProcess(parsed: K): K = parsed match {
    case KToken(str, KString, atts)          => KToken(StringEscapeUtils.unescapeJava(str.drop(1).dropRight(1)), KString, atts)
    case kt@KToken(_, _, _)                  => kt
    case KApply(head, Args(subNodes), atts) => KApply(head, Args(subNodes map preProcess), atts)
  }

  def resolveRules(module: Module): Module = {
    ???
  }
  //    val resolvedImports = (module.imports map resolveRules) + KML
  //    val sorts = module.localSorts
  //    val ruleSyntax = (module.localSorts map ((ls: org.kframework.kore.Sort) => Production(ls, KMLFormula, Att()))) ++ module.localSyntaxSentences
  //    val rulesToParse = module.localSemanticSentences
  //    Module(module.name, resolvedImports, module.localSentences ++ kmlSubsorts)
  //    // separate module from rules
  //    // import KML into module
  //    // subsort everything in module to KMLFormula
  //    // parse rules in resulting module as sort KMLFormula
  //    // put rules back into original module
  //  }


  //    private Module resolveNonConfigBubbles(Module module, Function<String, Module> getProcessedModule, boolean isStrict) {
  //        if (stream(module.localSentences())
  //                .filter(s -> s instanceof Bubble)
  //                .map(b -> (Bubble) b)
  //                .filter(b -> !b.sentenceType().equals("config")).count() == 0)
  //            return module;
  //        Module ruleParserModule = RuleGrammarGenerator.getRuleGrammar(module, getProcessedModule);
  //
  //        ParseCache cache = loadCache(ruleParserModule);
  //        ParseInModule parser = RuleGrammarGenerator.getCombinedGrammar(cache.getModule(), isStrict);
  //
  //        java.util.Set<Bubble> bubbles = stream(module.localSentences())
  //                .parallel()
  //                .filter(s -> s instanceof Bubble)
  //                .map(b -> (Bubble) b).collect(Collectors.toSet());
  //
  //        Set<Sentence> ruleSet = bubbles.stream()
  //                .filter(b -> b.sentenceType().equals("rule"))
  //                .map(b -> performParse(cache.getCache(), parser, b))
  //                .flatMap(r -> {
  //                    if (r.isRight()) {
  //                        return Stream.of(this.upRule(r.right().get()));
  //                    } else {
  //                        errors.addAll(r.left().get());
  //                        return Stream.empty();
  //                    }
  //                }).collect(Collections.toSet());
  //
  //        Set<Sentence> contextSet = bubbles.stream()
  //                .filter(b -> b.sentenceType().equals("context"))
  //                .map(b -> performParse(cache.getCache(), parser, b))
  //                .flatMap(r -> {
  //                    if (r.isRight()) {
  //                        return Stream.of(this.upContext(r.right().get()));
  //                    } else {
  //                        errors.addAll(r.left().get());
  //                        return Stream.empty();
  //                    }
  //                }).collect(Collections.toSet());
  //
  //        return Module(module.name(), module.imports(),
  //                stream((Set<Sentence>) module.localSentences().$bar(ruleSet).$bar(contextSet)).filter(b -> !(b instanceof Bubble)).collect(Collections.toSet()), module.att());
  //    }

}
