package org.kframework.minikore

import org.kframework.minikore.MiniKore._
import org.kframework.minikore.KoreToMini._


object MiniKoreOuterUtils {

  // Attributes
  // ==========

  // Apply Transformation to Attributes (Should be moved into MiniKore data-structures)
  // So each outer data-structure would implement "HasAttributes" or something, which would have
  // function `onAttributes` among others
  def onAttributesSent(f: Pattern => Pattern): Sentence => Sentence = {
    case Import(name, att)                         => Import(name, att map f)
    case SortDeclaration(sort, att)                => SortDeclaration(sort, att map f)
    case SymbolDeclaration(sort, label, args, att) => SymbolDeclaration(sort, label, args, att map f)
    case Rule(pattern, att)                        => Rule(pattern, att map f)
    case Axiom(pattern, att)                       => Axiom(pattern, att map f)
  }
  
  def onAttributesMod(f: Pattern => Pattern): Module => Module = {
    case Module(name, sentences, att) => Module(name, sentences map onAttributesSent(f), att map f)
  }
  
  def onAttributesDef(f: Pattern => Pattern): Definition => Definition = {
    case Definition(modules, att) => Definition(modules map onAttributesMod(f), att map f)
  }

  def getAttributeKey(key: String, atts: Attributes): Seq[Seq[Pattern]] = atts collect { case Application(`key`, args) => args }

}

object MiniKorePatternUtils {

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
  // consListLeft("apply", "4")(Seq("1","2","3")) => apply(1,apply(2,apply(3,4)))
  // consListRight("apply", "0")(Seq("1","2","3")) => apply(apply(apply(0,1),2),3)

  def consListLeft(cons: String, nil: String)(ps: Seq[Pattern]): Pattern = ps.foldRight(Application(nil, Seq.empty))((acc, next) => Application(cons, Seq(acc, next)))
  def consListRight(cons: String, nil: String)(ps: Seq[Pattern]): Pattern = ps.foldLeft(Application(nil, Seq.empty))((acc, next) => Application(cons, Seq(acc, next)))

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
  import MiniKorePatternUtils._

  // TODO: I would like to make the downers take things of type Application (instead of Pattern), but that
  // means that all of the recursively downed parts of the matched patterns have to be type annotated, which
  // is quite verbose and ugly. Should we make a new subsort of just Application in a trait called "MetaPattern"
  // or something like that?

  // Helpers for Symbols
  // ===================

  def makeDomainValueByName(name: String)(concrete: String): DomainValue = DomainValue(name, concrete)
  def getDomainValueByName(name: String): DomainValue => String = { case DomainValue(`name`, value) => value }

  val makeSymbol: String => DomainValue = makeDomainValueByName("KSymbol@KTOKENS")
  def getSymbol(parsed: Pattern): String = getDomainValueByName("KSymbol@KTOKENS")(downDomainValue(parsed))

  def makeSymbolList(concrete: Seq[String]): Pattern = consListLeft("KSymbolList", ".KSymbolList")(concrete map (cs => makeSymbol(cs)))
  def getSymbolList(parsed: Pattern): Seq[String] = flattenByLabels("KSymbolList", ".KSymbolList")(parsed) map (downDomainValue andThen { case DomainValue("KSymbol@KTOKENS", value) => value })

  // Patterns
  // ========

  val upDomainValue: DomainValue => Application = { case DomainValue(name, value) => Application("KMLDomainValue", Seq(Application(name, Nil), Application(value, Nil))) }
  val downDomainValue: Pattern => DomainValue = {
    case Application("KMLDomainValue", Application(name, Nil) :: Application(value, Nil) :: Nil) => DomainValue(name, value)
    //case Application(value, Nil) => DomainValue("KSymbol@KTOKENS", value) // TODO: remove/fix this case
  }
  val upSymbol: String => Application = makeSymbol andThen upDomainValue

  val upVariable: Variable => Application = { case Variable(name, sort) => Application("KMLVariable", Seq(makeSymbol(name), makeSymbol(sort))) }
  val downVariable: Pattern => Variable = { case Application("KMLVariable", DomainValue("KSymbol@KTOKENS", name) :: DomainValue("KSymbol@KTOKENS", sort) :: Nil) => Variable(name, sort) }

  val upPattern: Pattern => Application = {
    case Application(label, Nil)  => Application("KMLApplication", Seq(Application(label, Seq.empty)))
    case Application(label, args) => Application("KMLApplication", Seq(upSymbol(label), upPatternList(args)))
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
    case Application("KMLApplication", Application(label, Nil) :: Nil) => Application(label, Seq.empty)
    case Application("KMLApplication", label :: pList :: Nil)          => Application(getSymbol(label), downPatternList(pList))
    case Application("KMLTrue", Nil)                                   => True()
    case Application("KMLFalse", Nil)                                  => False()
    case Application("KMLAnd", p1 :: p2 :: Nil)                        => And(downPattern(p1), downPattern(p2))
    case Application("KMLOr", p1 :: p2 :: Nil)                         => Or(downPattern(p1), downPattern(p2))
    case Application("KMLNot", p :: Nil)                               => Not(downPattern(p))
    case Application("KMLImplies", p1 :: p2 :: Nil)                    => Implies(downPattern(p1), downPattern(p2))
    case Application("KMLExists", v :: p :: Nil)                       => Exists(downVariable(v), downPattern(p))
    case Application("KMLForall", v :: p :: Nil)                       => ForAll(downVariable(v), downPattern(p))
    case Application("KMLNext", p :: Nil)                              => Next(downPattern(p))
    case Application("KMLRewrite", p1 :: p2 :: Nil)                    => Rewrite(downPattern(p1), downPattern(p2))
    case Application("KMLEqual", p1 :: p2 :: Nil)                      => Equal(downPattern(p1), downPattern(p2))
    case vb@Application("KMLVariable", _)                              => downVariable(vb)
    case dv@Application("KMLDomainValue", _)                           => downDomainValue(dv)
  }

  def upPatternList(concretes: Seq[Pattern]): Pattern = consListLeft("KMLPatternList", ".KMLPatternList")(concretes map upPattern)
  def downPatternList(parsed: Pattern): Seq[Pattern] = flattenByLabels("KMLPatternList", ".KMLPatternList")(parsed) map downPattern

  // Sentences
  // =========

  val upAttributes: Attributes => Pattern = {
    case Nil          => Application(".KAttributes", Seq.empty)
    case concreteAtts => Application("KAttributes", Seq(upPatternList(concreteAtts)))
  }
  def downAttributes(parsed: Pattern): Attributes = flattenByLabels("KAttributes", ".KAttributes")(parsed) flatMap downPatternList

  val upSentence: Sentence => Pattern = {
    case Import(name, atts)                         => Application("KImport", Seq(makeSymbol(name), upAttributes(atts)))
    case SortDeclaration(sort, atts)                => Application("KSortDeclaration", Seq(makeSymbol(sort), upAttributes(atts)))
    case SymbolDeclaration(sort, label, args, atts) => Application("KSymbolDeclaration", Seq(makeSymbol(sort), makeSymbol(label), makeSymbolList(args), upAttributes(atts)))
    case Rule(pattern, atts)                        => Application("KRule", Seq(upPattern(pattern), upAttributes(atts)))
    case Axiom(pattern, atts)                       => Application("KAxiom", Seq(upPattern(pattern), upAttributes(atts)))
  }
  val downSentence: Pattern => Sentence = {
    case Application("KImport", importName :: atts :: Nil)        => Import(getSymbol(importName), downAttributes(atts))
    case Application("KSortDeclaration", sortName :: atts :: Nil) => SortDeclaration(getSymbol(sortName), downAttributes(atts))
    case Application("KSymbolDeclaration", sortName :: label :: args :: atts :: Nil)
                                                                  => SymbolDeclaration(getSymbol(sortName), getSymbol(label), getSymbolList(args), downAttributes(atts))
    case Application("KRule", rule :: atts :: Nil)                => dummySentence(Application(iBubble, Seq(S("rule"), S(getSymbol(rule).replaceAll("\\s+$", "").replaceAll("^\\s+^", "")))) +: downAttributes(atts))
    // TODO: This case should not be parsed like this, should it?
    case Application("KSymbolDeclaration", sortName :: Application(regex, Nil) :: atts :: Nil)
                                                                  => SymbolDeclaration(getSymbol(sortName), regex, Seq.empty, downAttributes(atts))
  }

  // Definitions
  // ===========

  val upModule: Module => Pattern = {
    case Module(name: String, sentences: Seq[Sentence], atts: Attributes) => Application("KModule", Seq(makeSymbol(name), consListLeft("KSentenceList", ".KSentenceList")(sentences map upSentence), upAttributes(atts)))
  }
  val downModule: Pattern => Module = {
    case Application("KModule", name :: sentences :: atts :: Nil) => Module(getSymbol(name), flattenByLabels("KSentenceList", ".KSentenceList")(sentences) map downSentence, downAttributes(atts))
  }

  val upDefinition: Definition => Pattern = {
     case Definition(modules: Seq[Module], atts: Attributes) => Application("KDefinition", Seq(upAttributes(atts), consListLeft("KModuleList", ".KModuleList")(modules map upModule)))
  }
  val downDefinition: Pattern => Definition = {
     case Application("KDefinition", atts :: modules :: Nil) => Definition(flattenByLabels("KModuleList", ".KModuleList")(modules) map downModule, downAttributes(atts))
  }
}
