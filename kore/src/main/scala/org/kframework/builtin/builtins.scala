package org.kframework.builtin

import org.kframework.attributes.Att
import org.kframework.builtin.Sorts._
import org.kframework.definition._
import org.kframework.kore
import org.kframework.kore.{Constructors, Sort}

object Sugar {

  case class BuildingProduction(seq: Seq[ProductionItem], att: Att) {
    def +(pi: ProductionItem) = BuildingProduction(seq :+ pi, att)
    def +(pi: Sort) = BuildingProduction(seq :+ NonTerminal(pi), att)
    def +(pi: String) = BuildingProduction(seq :+ Terminal(pi), att)
    def |(pair: (String, String)) = BuildingProduction(seq, att + pair)
    def |(s: String) = BuildingProduction(seq, att + s)
  }

  def makeKLabel(items: Seq[ProductionItem]): String = items match {
    case Seq(_: NonTerminal, Terminal(string)) => string
    case Seq(_: NonTerminal, Terminal(string), _: NonTerminal) => string
    case items => items map {
      case NonTerminal(sort) => "_"
      case Terminal(string) => string
      //TODO(cos): remove this
      case RegexTerminal(regex) => "regexp"
    } mkString
  }


  implicit def SortWithBuildingProduction(sort: Sort) = BuildingProduction(Seq(NonTerminal(sort)), Att())
  implicit def SortWithBuildingProduction(s: String) = BuildingProduction(Seq(Terminal(s)), Att())

  implicit class BuildingSyntax(sort: Sort) {
    def ::=(bp: BuildingProduction) = Production(makeKLabel(bp.seq), sort, bp.seq, bp.att)
  }

  val production: Production = K ::= K + "~>" + K
}

import org.kframework.builtin.Sugar._

object Labels {
  val Hole = "HOLE"
  //  val KBag = "KBag"
  val And = "_andBool_"
  val Or = "_orBool_"
}

class Labels[K <: kore.K](cons: Constructors[K]) {

  lazy val Hole = KStuff.Hole
  //  lazy val KBag = KLabel(Labels.KBag)
}

object KStuff extends Module("K-STUFF", Set(), Set(
  K ::= "HOLE"
)) {
  val Hole = KLabel("HOLE")
}

object Tuple extends Module("TUPLE", Set(), Set(
  K ::= "(" + K + "," + K + ")"
)) {
  val _2 = KLabel("(_,_)")
}

object BoolModule extends Module("BOOLEAN", Set(), Set(
  Bool ::= Bool + "&&" + Bool | "hook" -> "#BOOL:_&&_",
  Bool ::= Bool + "||" + Bool | "hook" -> "#BOOL:_||_",
  Bool ::= "!" + Bool | "hook" -> "#BOOL:!_"
)) {
  val && = KLabel("&&")
  val || = KLabel("||")
  val ! = KLabel("!")
}

object Int extends Module("INT", Set(), Set(
  Bool ::= Bool + "+" + Bool | "hook" -> "#INT:_+_",
  Bool ::= Bool + "-" + Bool | "hook" -> "#INT:_-_",
  Bool ::= Bool + "*" + Bool | "hook" -> "#INT:_*_",
  Bool ::= Bool + "/" + Bool | "hook" -> "#INT:_/_"
)) {
  val + = KLabel("_+_")
  val - = KLabel("_-_")
  val / = KLabel("_/_")
  val * = KLabel("_*_")
}

object KModule extends Module("K", Set(), Set(
  Production("_=>_", Sorts.K,
    Seq(NonTerminal(Sorts.K), Terminal("=>"), NonTerminal(Sorts.K)), Att())
)) {
  val KRewrite = KLabel("_=>_")
}

object KSeq extends Module("K-SEQ", Set(), Set(
  K ::= K + "~>" + K
)) {
  val KSeq = KLabel("~>")
}

object KAST extends Module("KAST", Set(), Set())

object TheVarModule extends Module("THE-VAR-MODULE", Set(), Set())

object Bag extends Module("BAG", Set(), Set()) {
  val Bag = KLabel("Bag")
}