package org.kframework.definition

import org.kframework.attributes.Att
import org.kframework.builtin.Sorts
import org.kframework.kore.ADT._
import org.kframework.kore._
import org.kframework.meta.Down

import collection.JavaConverters._
//import collection.immutable.Set._

/**
  * Created by lpena on 10/11/16.
  */

object KParserBootstrapDSL {

  implicit def BecomingNonTerminal(s: ADT.SortLookup): NonTerminal = NonTerminal(s)
  implicit def BecomingTerminal(s: String): Terminal = Terminal(s)
  implicit def BecomingSequence(ps: ProductionItem*): Seq[ProductionItem] = ps

  def Sort(s: String): ADT.SortLookup = ADT.SortLookup(s)

  def regex(s: String): ProductionItem = RegexTerminal("#", s, "#")

  case class syntax(s: ADT.SortLookup) {
    def is(pis: ProductionItem*): BecomingSyntax = BecomingSyntax(s, pis)
  }

  case class BecomingSyntax(sort: ADT.SortLookup, pis: Seq[ProductionItem]) {
    def att(atts: K*): Production = Production(sort, pis, atts.foldLeft(Att())(_+_))
  }

  implicit def syntaxWithoutAttributes(bp: BecomingSyntax) : Production =
    Production(bp.sort, bp.pis, Att())

  def asKApply(label: String, values: List[String]): K =
    KORE.KApply(KORE.KLabel(label), KORE.KList(values map { value => KORE.KToken(value, Sorts.KString, Att()) }), Att())

  def imports(s: Module*): Set[Module] = s.toSet
  def sentences(s: Sentence*): Set[Sentence] = s.toSet
  def klabel(label: String): K = asKApply("klabel", List(label))
  def ktoken(label: String): K = asKApply("ktoken", List(label))
  implicit def constAtt(str : String): K = asKApply(str, List.empty)

  val KORE_STRING =
    """
    module SORT-K
      syntax K [hook(K.K)]
    endmodule

    module BASIC-K
      imports SORT-K
      syntax KLabel
      syntax KItem        [hook(K.KItem)]
      syntax K ::= KItem  [allowChainSubsort]
      syntax KConfigVar
      syntax KBott
      syntax KList
      syntax KResult
      syntax MetaVariable
      syntax Bottom
    endmodule

    module KSTRING
      syntax KString ::= r"[\\\"](([^\\\"\n\r\\\\])|([\\\\][nrtf\\\"\\\\])|([\\\\][x][0-9a-fA-F]{2})|([\\\\][u][0-9a-fA-F]{4})|([\\\\][U][0-9a-fA-F]{8}))*[\\\"]"      [token, hook(org.kframework.kore.KString)]
    endmodule
    """

  val REST_STRING =
    """
    module KAST
      imports BASIC-K
      imports KSTRING
      syntax KBott ::= "#token" "(" KString "," KString ")"  [klabel(#KToken), hook(org.kframework.kore.KToken)]
                     | "#klabel" "(" KLabel ")"              [klabel(#InjectedKLabel), hook(org.kframework.kore.InjectedKLabel)]
                     | KLabel "(" KList ")"                  [klabel(#KApply), hook(org.kframework.kore.KApply)]
      syntax KItem ::= KBott                                 [allowChainSubsort]

      syntax KLabel ::= r"`(\\\\`|\\\\\\\\|[^`\\\\\n\r])+`" [token, hook(org.kframework.kore.KLabel)]
                      | r"(?<![a-zA-Z0-9])[#a-z][a-zA-Z0-9@\\-]*"               [token, hook(org.kframework.kore.KLabel), autoReject]

      syntax KList ::= K                 [allowChainSubsort]
                     | ".KList"          [klabel(#EmptyKList), hook(org.kframework.kore.EmptyKList)]
                     | ".::KList"        [klabel(#EmptyKList), hook(org.kframework.kore.EmptyKList)]
                     | KList "," KList   [klabel(#KList), left, assoc, unit(#EmptyKList), hook(org.kframework.kore.KList), prefer]
    //TODO(dwightguth): there is some kind of weird issue with this production which is causing AddEmptyLists to behave incorrectly.
    // we need to fix this issue so we can add this production back.
    //                 | KList ",," KList  [klabel(#KList), left, assoc, unit(#EmptyKList), hook(org.kframework.kore.KList), prefer]
    endmodule

    // To be used when parsing/pretty-printing ground configurations
    module KSEQ
      imports KAST
      imports K-TOP-SORT
      syntax KBott ::= ".K"      [klabel(#EmptyK), hook(org.kframework.kore.EmptyK)]
                     | "."       [klabel(#EmptyK), hook(org.kframework.kore.EmptyK)]
                     | ".::K"    [klabel(#EmptyK), hook(org.kframework.kore.EmptyK)]
                     | K "~>" K  [klabel(#KSequence), left, assoc, unit(#EmptyK), hook(org.kframework.kore.KSequence)]
      syntax left #KSequence
      syntax KBott     ::= "(" K ")"    [bracket]
    endmodule

    // To be used when parsing/pretty-printing symbolic configurations
    module KSEQ-SYMBOLIC
      imports KSEQ
      syntax #KVariable ::= r"(?<![A-Za-z0-9_\\$!\\?])(\\!|\\?)?([A-Z][A-Za-z0-9'_]*|_)"   [token, autoReject, hook(org.kframework.kore.KVariable)]
      syntax KConfigVar ::= r"(?<![A-Za-z0-9_\\$!\\?])(\\$)([A-Z][A-Za-z0-9'_]*)"          [token, autoReject]
      syntax KBott      ::= #KVariable [allowChainSubsort]
      syntax KBott      ::= KConfigVar [allowChainSubsort]
      syntax KLabel     ::= #KVariable [allowChainSubsort]
    endmodule

    module KCELLS
      imports KAST

      syntax Cell
      syntax Bag ::= Bag Bag  [left, assoc, klabel(#cells), unit(#cells)]
                   | ".Bag"   [klabel(#cells)]
                   | ".::Bag" [klabel(#cells)]
                   | Cell     [allowChainSubsort]
      syntax Bag ::= "(" Bag ")" [bracket]
      syntax K ::= Bag
      syntax Bag ::= KBott
    endmodule

    module RULE-CELLS
      imports KCELLS
      // if this module is imported, the parser automatically
      // generates, for all productions that have the attribute 'cell' or 'maincell',
      // a production like below:
      //syntax Cell ::= "<top>" #OptionalDots K #OptionalDots "</top>" [klabel(<top>)]

      syntax #OptionalDots ::= "..." [klabel(#dots)]
                             | ""    [klabel(#noDots)]
    endmodule

    module RULE-PARSER
      imports RULE-LISTS
      imports RULE-CELLS
      // imported in modules which generate rule parsers
      // TODO: (radumereuta) don't use modules as markers to generate parsers
    endmodule

    module CONFIG-CELLS
      imports KCELLS
      imports RULE-LISTS
      syntax #CellName ::= r"[a-zA-Z0-9\\-]+"  [token]

      syntax Cell ::= "<" #CellName #CellProperties ">" K "</" #CellName ">" [klabel(#configCell)]
      syntax Cell ::= "<" #CellName "/>" [klabel(#externalCell)]
      syntax Cell ::= "<br" "/>" [klabel(#breakCell)]

      syntax #CellProperties ::= #CellProperty #CellProperties [klabel(#cellPropertyList)]
                               | ""                            [klabel(#cellPropertyListTerminator)]
      syntax #CellProperty ::= #CellName "=" KString           [klabel(#cellProperty)]

    endmodule

    module REQUIRES-ENSURES
      imports BASIC-K

      syntax RuleContent ::= K                                 [klabel("#ruleNoConditions"), allowChainSubsort, latex({#1}{}{})]
                           | K "requires" K                    [klabel("#ruleRequires"), latex({#1}{#2}{})]
                           | K "when" K                        [klabel("#ruleRequires"), latex({#1}{#2}{})]
                           | K "ensures"  K                    [klabel("#ruleEnsures"), latex({#1}{}{#3})]
                           | K "requires" K "ensures" K        [klabel("#ruleRequiresEnsures"), latex({#1}{#2}{#3})]
                           | K "when" K "ensures" K            [klabel("#ruleRequiresEnsures"), latex({#1}{#2}{#3})]
    endmodule

    module K-TOP-SORT
      // if this module is imported, the parser automatically
      // generates, for all sorts, productions of the form:
      // K     ::= Sort
      // this is part of the mechanism that allows concrete user syntax in K
    endmodule

    module K-BOTTOM-SORT
      // if this module is imported, the parser automatically
      // generates, for all sorts, productions of the form:
      // Sort  ::= KBott
      // this is part of the mechanism that allows concrete user syntax in K
    endmodule

    module K-SORT-LATTICE
      imports K-TOP-SORT
      imports K-BOTTOM-SORT
    endmodule

    module AUTO-CASTS
      // if this module is imported, the parser automatically
      // generates, for all sorts, productions of the form:
      // Sort  ::= Sort "::Sort"
      // Sort  ::= Sort ":Sort"
      // KBott ::= Sort "<:Sort"
      // Sort  ::= K    ":>Sort"
      // this is part of the mechanism that allows concrete user syntax in K
    endmodule

    module AUTO-FOLLOW
      // if this module is imported, the parser automatically
      // generates a follow restriction for every terminal which is a prefix
      // of another terminal. This is useful to prevent ambiguities such as:
      // syntax K ::= "a"
      // syntax K ::= "b"
      // syntax K ::= "ab"
      // syntax K ::= K K
      // #parse("ab", "K")
      // In the above example, the terminal "a" is not allowed to be followed by a "b"
      // because it would turn the terminal into the terminal "ab".
    endmodule

    module PROGRAM-LISTS
      imports SORT-K
      // if this module is imported, the parser automatically
      // replaces the default productions for lists:
      // Es ::= E "," Es [userList("*"), klabel('_,_)]
      //      | ".Es"    [userList("*"), klabel('.Es)]
      // into a series of productions more suitable for programs:
      // Es#Terminator ::= ""      [klabel('.Es)]
      // Ne#Es ::= E "," Ne#Es     [klabel('_,_)]
      //         | E Es#Terminator [klabel('_,_)]
      // Es ::= Ne#Es
      //      | Es#Terminator      // if the list is *
    endmodule

    module RULE-LISTS
      // if this module is imported, the parser automatically
      // adds the subsort production to the parsing module only:
      // Es ::= E        [userList("*")]

    endmodule

    module DEFAULT-CONFIGURATION
      imports BASIC-K

      configuration <k> $PGM:K </k>
    endmodule

    // To be used to parse semantic rules
    module K
      imports KSEQ-SYMBOLIC
      imports REQUIRES-ENSURES
      imports K-SORT-LATTICE
      imports AUTO-CASTS
      imports AUTO-FOLLOW
      syntax KBott     ::= K "=>" K     [klabel(#KRewrite), hook(org.kframework.kore.KRewrite), non-assoc]
      syntax non-assoc #KRewrite

    endmodule

    // To be used to parse terms in full K
    module K-TERM
      imports KSEQ-SYMBOLIC
      imports K-SORT-LATTICE
      imports AUTO-CASTS
      imports AUTO-FOLLOW
    endmodule
    """

  //val KRegexString = "[\\\"].*[\\\"]"
  val KRegexString =  "[\\\"]([^\\\"\n\r\\\\]|[\\\\][nrtf\\\"\\\\])*[\\\"]"
  val KRegexString2 = "[\\\"]([^\\\"\\n\\r\\\\]|[\\\\][nrtf\\\"\\\\])*[\\\"]"
  //val KRegexString = "[\\\"]([^\\\"]|[\\\"])*[\\\"]"
//  val KRegexString = "[\\\"](([^\\\"\n\r\\\\])" +
//                           "|([\\\\][nrtf\\\"\\\\])" +
//                           "|([\\\\][x][0-9a-fA-F]{2})" +
//                           "|([\\\\][u][0-9a-fA-F]{4})" +
//                           "|([\\\\][U][0-9a-fA-F]{8})" +
//                           ")*" +
//                     "[\\\"]"
  val KRegexSort = "[A-Z][A-Za-z0-9]*"
  val KRegexAttributeKey = "[\\.A-Za-z\\-0-9]*"
  val KRegexModuleName = "[A-Z][A-Z\\-]*"
  def rString(str: String) : String = "r\"" + str + "\""

  val KTOKENS_STRING =
    """
    module KTOKENS
      syntax KString ::= """ + rString(KRegexString2) + """ [token, klabel(KString, .KKeyList), .KAttributes]
      syntax KSort ::= """ + rString(KRegexSort) + """ [token, klabel(KSort, .KKeyList), .KAttributes]
      syntax KAttributeKey ::= """ + rString(KRegexAttributeKey) + """ [token, klabel(KAttributeKey, .KKeyList), .KAttributes]
      syntax KModuleName ::= """ + rString(KRegexModuleName) + """ [token, klabel(KModuleName, .KKeyList), .KAttributes]
    endmodule
    """

  val KString = Sort("KString")
  val KSort = Sort("KSort")
  val KAttributeKey = Sort("KAttributeKey")
  val KModuleName = Sort("KModuleName")

  val KTOKENS = Module("KTOKENS", imports(), sentences(

    syntax(KString) is regex(KRegexString) att ("token", klabel("KString")),
    syntax(KSort) is regex(KRegexSort) att ("token", klabel("KSort")),
    syntax(KAttributeKey) is regex(KRegexAttributeKey) att ("token", klabel("KAttributeKey")),
    syntax(KModuleName) is regex(KRegexModuleName) att ("token", klabel("KModuleName"))

  ))

  val KML_STRING =
    """
    module KML
      imports KTOKENS

      syntax KMLVar ::= "kmlvar" "(" KString ")" [klabel(kmlvar, .KKeyList), .KAttributes]
      syntax KMLFormula ::= KMLVar [.KAttributes]
      syntax KMLFormula ::= "KMLtrue" [klabel(KMLtrue, .KKeyList), .KAttributes]
      syntax KMLFormula ::= "KMLfalse" [klabel(KMLfalse, .KKeyList), .KAttributes]
      syntax KMLFormula ::= KMLFormula "KMLand" KMLFormula [klabel(KMLand, .KKeyList), .KAttributes]
      syntax KMLFormula ::= KMLFormula "KMLor" KMLFormula [klabel(KMLor, .KKeyList), .KAttributes]
      syntax KMLFormula ::= "KMLnot" KMLFormula [klabel(KMLnot, .KKeyList), .KAttributes]
      syntax KMLFormula ::= "KMLexists" KMLVar "." KMLFormula [klabel(KMLexists, .KKeyList), .KAttributes]
      syntax KMLFormula ::= "KMLforall" KMLVar "." KMLFormula [klabel(KMLforall, .KKeyList), .KAttributes]
      syntax KMLFormula ::= KMLFormula "KML=>" KMLFormula [klabel(KMLnext, .KKeyList), .KAttributes]
    endmodule
    """

  val KMLVar = Sort("KMLVar")
  val KMLFormula = Sort("KMLFormula")

  val KML = Module("KML", imports(KTOKENS), sentences(

    syntax(KMLVar) is ("kmlvar", "(", KString, ")") att klabel("kmlvar"),

    syntax(KMLFormula) is KMLVar,
    syntax(KMLFormula) is "KMLtrue" att klabel("KMLtrue"),
    syntax(KMLFormula) is "KMLfalse" att klabel("KMLfalse"),
    syntax(KMLFormula) is (KMLFormula, "KMLand", KMLFormula) att klabel("KMLand"),
    syntax(KMLFormula) is (KMLFormula, "KMLor", KMLFormula) att klabel("KMLor"),
    syntax(KMLFormula) is ("KMLnot", KMLFormula) att klabel("KMLnot"),
    syntax(KMLFormula) is ("KMLexists", KMLVar, ".", KMLFormula) att klabel("KMLexists"),
    syntax(KMLFormula) is ("KMLforall", KMLVar, ".", KMLFormula) att klabel("KMLforall"),
    syntax(KMLFormula) is (KMLFormula, "KML=>", KMLFormula) att klabel("KMLnext")

  ))

  val KATTRIBUTES_STRING =
    """
    module KATTRIBUTES
      imports KTOKENS

      syntax KKeyList ::= ".KKeyList" [klabel(.KKeyList, .KKeyList), .KAttributes]
      syntax KKeyList ::= KAttributeKey "," KKeyList [klabel(KKeyList, .KKeyList), .KAttributes]
      syntax KAttribute ::= KAttributeKey [.KAttributes]
      syntax KAttribute ::= KAttributeKey "(" KKeyList ")" [klabel(KAttributeApply, .KKeyList), .KAttributes]
      syntax KAttributes ::= ".KAttributes" [klabel(.KAttributes, .KKeyList), .KAttributes]
      syntax KAttributes ::= KAttribute "," KAttributes [klabel(KAttributes, .KKeyList), .KAttributes]
    endmodule
    """

  val KKeyList = Sort("KKeyList")
  val KAttribute = Sort("KAttribute")
  val KAttributes = Sort("KAttributes")

  val KATTRIBUTES = Module("KATTRIBUTES", imports(KTOKENS), sentences(

    syntax(KKeyList) is ".KKeyList" att klabel(".KKeyList"),
    syntax(KKeyList) is (KAttributeKey, ",", KKeyList) att klabel("KKeyList"),


    syntax(KAttribute) is KAttributeKey,
    syntax(KAttribute) is (KAttributeKey, "(", KKeyList, ")") att klabel("KAttributeApply"),
    syntax(KAttributes) is ".KAttributes" att klabel(".KAttributes"),
    syntax(KAttributes) is (KAttribute, ",", KAttributes) att klabel("KAttributes")

  ))

  val KSENTENCES_STRING =
    """
    module KSENTENCES
      imports KATTRIBUTES
      imports KML

      syntax KImport ::= "imports" KModuleName [klabel(KImport, .KKeyList), .KAttributes]
      syntax KImportList ::= "" [klabel(.KImportList, .KKeyList), .KAttributes]
      syntax KImportList ::= KImport KImportList [klabel(KImportList, .KKeyList), .KAttributes]
      syntax KTerminal ::= KString [.KAttributes]
      syntax KTerminal ::= "r" KString [klabel(KRegex, .KKeyList), .KAttributes]
      syntax KNonTerminal ::= KSort [.KAttributes]
      syntax KProductionItem ::= KTerminal [.KAttributes]
      syntax KProductionItem ::= KNonTerminal [.KAttributes]
      syntax KProduction ::= KProductionItem [.KAttributes]
      syntax KProduction ::= KProductionItem KProduction [klabel(KProduction, .KKeyList), .KAttributes]
      syntax KSentence ::= "syntax" KSort [klabel(KSortDecl, .KKeyList), .KAttributes]
      syntax KSentence ::= "syntax" KSort "::=" KProduction "[" KAttributes "]" [klabel(KSentenceSyntax, .KKeyList), .KAttributes]
      syntax KSentenceList ::= "" [klabel(.KSentenceList, .KKeyList), .KAttributes]
      syntax KSentenceList ::= KSentence KSentenceList [klabel(KSentenceList, .KKeyList), .KAttributes]
    endmodule
    """

  val KImport = Sort("KImport")
  val KImportList = Sort("KImportList")

  val KTerminal = Sort("KTerminal")
  val KNonTerminal = Sort("KNonTerminal")
  val KProductionItem = Sort("KProductionItem")
  val KProduction = Sort("KProduction")

  val KSentence = Sort("KSentence")
  val KSentenceList = Sort("KSentenceList")

  val KSENTENCES = Module("KSENTENCES", imports(KATTRIBUTES, KML), sentences(

    syntax(KImport) is ("imports", KModuleName) att klabel("KImport"),
    syntax(KImportList) is "" att klabel(".KImportList"),
    syntax(KImportList) is (KImport, KImportList) att klabel("KImportList"),

    syntax(KTerminal) is KString,
    syntax(KTerminal) is ("r", KString) att klabel("KRegex"),
    syntax(KNonTerminal) is KSort,
    syntax(KProductionItem) is KTerminal,
    syntax(KProductionItem) is KNonTerminal,
    syntax(KProduction) is KProductionItem,
    syntax(KProduction) is (KProductionItem, KProduction) att klabel("KProduction"),
    syntax(KSentence) is ("syntax", KSort) att klabel("KSortDecl"),
    syntax(KSentence) is ("syntax", KSort, "::=", KProduction, "[", KAttributes, "]") att klabel("KSentenceSyntax"),

    syntax(KSentenceList) is "" att klabel(".KSentenceList"),
    syntax(KSentenceList) is (KSentence, KSentenceList) att klabel("KSentenceList")

  ))

  val KDEFINITION_STRING =
    """
    module KDEFINITION
      imports KSENTENCES

      syntax KModule ::= "module" KModuleName KImportList KSentenceList "endmodule" [klabel(KModule, .KKeyList), .KAttributes]
      syntax KModuleList ::= "" [klabel(.KModuleList, .KKeyList), .KAttributes]
      syntax KModuleList ::= KModule KModuleList [klabel(KModuleList, .KKeyList), .KAttributes]
      syntax KRequire ::= "require" KString [klabel(KRequire, .KKeyList), .KAttributes]
      syntax KRequireList ::= "" [klabel(.KRequireList, .KKeyList), .KAttributes]
      syntax KRequireList ::= KRequire KRequireList [klabel(KRequireList, .KKeyList), .KAttributes]
      syntax KDefinition ::= KRequireList KModuleList [klabel(KDefinition, .KKeyList), .KAttributes]
    endmodule
    """

  val KModule = Sort("KModule")
  val KModuleList = Sort("KModuleList")

  val KRequire = Sort("KRequire")
  val KRequireList = Sort("KRequireList")
  val KDefinition = Sort("KDefinition")

  val KDEFINITION = Module("KDEFINITION", imports(KSENTENCES), sentences(

    syntax(KModule) is ("module", KModuleName, KImportList, KSentenceList, "endmodule") att klabel("KModule"),
    syntax(KModuleList) is "" att klabel(".KModuleList"),
    syntax(KModuleList) is (KModule, KModuleList) att klabel("KModuleList"),

    syntax(KRequire) is ("require", KString) att klabel("KRequire"),
    syntax(KRequireList) is "" att klabel(".KRequireList"),
    syntax(KRequireList) is (KRequire, KRequireList) att klabel("KRequireList"),

    syntax(KDefinition) is (KRequireList, KModuleList) att klabel("KDefinition")

  ))

  val ALL_DEFS_STRING = KML_STRING + "\n" + KATTRIBUTES_STRING + "\n" + KSENTENCES_STRING + "\n" + KDEFINITION_STRING

  val EXP_STRING =
    """
    module EXP
      syntax Exp ::= "0" [.KAttributes]
      syntax Exp ::= "1" [.KAttributes]
      syntax Exp ::= Exp "+" Exp [.KAttributes]
      syntax Exp ::= Exp "*" Exp [.KAttributes]
    endmodule
    """

  val Exp = Sort("Exp")

  val EXP = Module("EXP", imports(), sentences(

    syntax(Exp) is "0" att klabel("0"),
    syntax(Exp) is "1" att klabel("1"),
    syntax(Exp) is (Exp, "+", Exp) att klabel("_+_"),
    syntax(Exp) is (Exp, "*", Exp) att klabel("_*_")

  ))

}

object KParserBootstrapDown {

  import KParserBootstrapDSL._

  def getASTNodes(parsed: K, nodeLabel: String): List[K] = parsed match {
    case node@KApply(nl, klist, _) => klist.items.asScala.flatMap(x => getASTNodes(x, nodeLabel)).toList ++ (if (nl == KLabelLookup(nodeLabel)) List(node) else List.empty)
    case _ => List.empty
  }
  def getASTNodes(parsed: K, nodeLabels: String*): List[K] = nodeLabels.foldLeft(List.empty: List[K]) ((acc, nL) => acc ++ getASTNodes(parsed, nL))

  def getASTTokens(parsed: K, tokenName: ADT.SortLookup): List[K] = parsed match {
    case KApply(_, KList(rest), _) => rest flatMap (x => getASTTokens(x, tokenName))
    case token@KToken(_, tn, _) if tokenName == tn => List(token)
    case _ => List.empty
  }

  def getASTModules(parsed: K): Set[K] = getASTNodes(parsed, "KModule") toSet

  def decomposeModule(parsedModule: K): (String, Set[String], Set[K]) = parsedModule match {
    case module@KApply(KLabelLookup("KModule"), KList(KToken(name, _, _) :: importList :: _), _) =>
      (name, getASTNodes(importList, "KImport") map { case KApply(_, KList(KToken(mn, _, _) :: _), _) => mn } toSet, getASTNodes(parsedModule, "KSentenceSyntax") toSet)
  }

  def downAttribute(attr: K): K = attr match {
    case KApply(KLabelLookup("KAttributeApply"), KList(KToken(fnc, KAttributeKey, _) :: keyList :: _), _)
    => asKApply(fnc, getASTTokens(keyList, KAttributeKey) map { case KToken(arg, KAttributeKey, _) => arg })
    case _ => attr
  }

  def downAttributes(parsedAttributes: K): Att =
    getASTNodes(parsedAttributes, "KAttribute", "KAttributeApply").foldLeft(Att()) ((acc, curr) => acc.add(downAttribute(curr)))

  def productionSeq(prod: K): Seq[ProductionItem] = prod match {
    case KApply(KLabelLookup("KProduction"), KList(KToken(str, KString, _) :: rest :: _), _) => Terminal(str.drop(1).dropRight(1)) +: productionSeq(rest)
    case KToken(str, KString, _) => Seq(Terminal(str.drop(1).dropRight(1)))
    case KApply(KLabelLookup("KProduction"), KList(KToken(sortName, KSort, _) :: rest :: _), _) => NonTerminal(Sort(sortName)) +: productionSeq(rest)
    case KToken(sortName, KSort, _) => Seq(NonTerminal(Sort(sortName)))
    case KApply(KLabelLookup("KProduction"), KList(KApply(KLabelLookup("KRegex"), KList(KToken(str, KString, _) :: _), _) :: rest :: _), _) => Terminal(str.drop(1).dropRight(1)) +: productionSeq(rest)
  }

  def downSentence(parsedSentence: K): Production = parsedSentence match {
    case KApply(KLabelLookup("KSentenceSyntax"), KList(KToken(sortName, _, _) :: production :: atts :: _), _) =>
      Production(Sort(sortName), productionSeq(production), downAttributes(atts))
    case KApply(KLabelLookup("KSortDecl"), KList(KToken(sortName, _, _) :: atts :: _), _) =>
      Production(Sort(sortName), Seq.empty, downAttributes(atts))
  }

  def getAllDownModules(parsed: K, builtins: Map[String, Module]): Map[String, Module] = {
    var decomposedModules = getASTNodes(parsed, "KModule") map decomposeModule
    var downedModules = builtins

    while (decomposedModules.nonEmpty) {
      val (moduleName, importSet, sentences) = decomposedModules.find { case (_, importSet, _) => importSet subsetOf downedModules.keySet }.get
      decomposedModules = decomposedModules.filterNot { case (mn, _, _) => mn == moduleName }
      downedModules += moduleName -> Module(moduleName, importSet map downedModules, sentences map downSentence)
    }
    downedModules
  }

  def getAllDownModules(parsed: K): Map[String, Module] = getAllDownModules(parsed, Map.empty: Map[String, Module])

  def testDown(parsed: K): Any = {
    val downer = Down(Set("scala.collection.immutable", "org.kframework.attributes"))
    downer(parsed)
  }

//  def getHookName(node: K): String = node match {
//    case KApply(KLabelLookup("hook"), KList(KToken(hookName, KAttributeKey, _) :: _), _) => hookName
//  }
//
//
////  def down[A](node: K): A = node match {
////    case KApply(_, KList(subNodes), atts) if atts contains "hook" => hook(getHookName(atts.getK("hook").get), subNodes)
////    case KApply(KLabelLookup(hookName), KList(subNodes), _) => hook(hookName, subNodes)
////  }
//
//  def downModule(subNodes: List[K]): Module = ???
//
//  def downSentence (subNodes: List[K]): Production = subNodes match {
//    case KApply(KLabelLookup(sortName), _, _) :: prod :: attributes :: nil =>
//      Production("KSentence", Sort(sortName), productionSeq(prod), downAttributes(attributes))
//  }
//
//
//  def down(parsed: K): Any = parsed match {
//    case KApply(KLabelLookup("KSentenceSyntax"), KList(KToken(sortName, _, _) :: production :: atts :: _), _)
//      => Production(Sort(sortName), productionSeq(production), down(atts))
//    case parsedAttributes@KApply(KLabelLookup("KAttributes"), _, _)
//      => getASTNodes(parsedAttributes, "KAttribute", "KAttributeApply").foldLeft(Att()) ((acc, curr) => acc.add(down(curr)))
//    case KApply(KLabelLookup("KAttributeApply"), KList(KToken(fnc, KAttributeKey, _) :: keyList :: _), _)
//      => asKApply(fnc, getASTTokens(keyList, KAttributeKey) map { case KToken(arg, KAttributeKey, _) => arg })
//  }
//
//  def hook(hookName: String): (List[K] => Any) = hookName match {
//    //case ("KModule", moduleName :: importList :: sentenceList :: nil) => ???
//    case "KModule" => downModule
//    case "KSentence" => downSentence
//    //case ("KSentence", sort :: prod :: attributes :: nil) =>
//      ///Production("KSentence", down[ADT.Sort](sort), down[Seq[ProductionItem]](prod), down[Att](attributes))
//      //syntax(down[ADT.SortLookup](sort)) is down[ProductionItem](prod) down[Att](attributes)

//  }



}
