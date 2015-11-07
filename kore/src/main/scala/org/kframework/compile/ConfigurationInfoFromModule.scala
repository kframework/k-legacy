package org.kframework.compile

import java.util

import com.google.common.collect.BiMap
import org.kframework.POSet
import org.kframework.kore.KORE.{KLabel, KApply}
import org.kframework.kore.Sort

import scala.collection.JavaConverters._

import org.kframework.compile.ConfigurationInfo.Multiplicity
import org.kframework.definition.{Module, NonTerminal, Production}
import org.kframework.kore._
import org.kframework.TopologicalSort._
import collection._

object ConfigurationInfoFromModule

class ConfigurationInfoFromModule(val m: Module) extends ConfigurationInfo {
  private val cellProductions: Map[Sort, Production] =
    m.productions.filter(_.att.contains("cell")).map(p => (p.sort, p)).toMap
  private val cellSorts: Set[Sort] = cellProductions.keySet

  private val cellBagProductions: Map[Sort, Production] =
    m.productions.filter(_.att.contains("assoc")).map(p => (p.sort, p)).toMap

  private val assocSorts: Set[Sort] = cellBagProductions.keySet

  private val cellToBagSort: Map[Sort, Sort] =
    cellSorts.flatMap((n: Sort) => {
      val sorts = assocSorts.filter(m.subsorts.directlyLessThan(n, _))
      assert(sorts.size <= 1, "Too many cell bags found for cell sort: " + n + ", " + sorts)
      if (sorts.nonEmpty) {
        Seq((n, sorts.head))
      } else {
        Seq()
      }
    }).toMap
  private val cellBagToCell: Map[Sort, Sort] = cellToBagSort.map(_.swap)
  private val cellBagSorts: Set[Sort] = cellBagToCell.keySet

  private val cellLabels: Map[Sort, KLabel] = cellProductions.mapValues(_.klabel.get)
  private val cellLabelsToSorts: Map[KLabel, Sort] = cellLabels.map(_.swap)

  private val cellFragmentLabel: Map[String, KLabel] =
    m.productions.filter(_.att.contains("cellFragment"))
      .map(p => (p.att.get("cellFragment", classOf[String]).get, p.klabel.get)).toMap
  private val cellToFragment: Map[Sort, Sort] =
    m.productions.filter(_.att.contains("cellFragment"))
      .map(p => (ADT.Sort(p.att.get("cellFragment", classOf[String]).get), p.sort)).toMap
  private val cellFragmentToCell: Map[Sort, Sort] =
    cellToFragment.map(_.swap)

  private val cellAbsentLabel: Map[Sort, KLabel] =
    m.productions.filter(_.att.contains("cellOptAbsent"))
      .map(p => (ADT.Sort(p.att.get("cellOptAbsent", classOf[String]).get), p.klabel.get)).toMap
  private val cellOptToCell: Map[Sort, Sort] =
    m.productions.filter(_.att.contains("cellOptAbsent"))
      .map(p => (p.sort, ADT.Sort(p.att.get("cellOptAbsent", classOf[String]).get))).toMap
  private val cellToCellOpt: Map[Sort, Sort] =
    cellOptToCell.map(_.swap)

  private val cellInitializer: Map[Sort, KApply] =
    m.productions.filter(p => cellSorts(p.sort) && p.att.contains("initializer"))
      .map(p => (p.sort, KApply(p.klabel.get))).toMap

  private val edges: Set[(Sort, Sort)] = cellProductions.toList.flatMap { case (s, p) =>
    p.items.flatMap {
      case NonTerminal(n) if cellSorts.contains(n) => List((s, n))
      case NonTerminal(n) if cellBagSorts.contains(n) => List((s, cellBagToCell(n)))
      case _ => List()
    }
  }.toSet

  private val edgesPoset: POSet[Sort] = POSet(edges)
  private val topCells = cellSorts.filter(l => !edges.map(_._2).contains(l))

  if (topCells.size > 1)
    throw new AssertionError("Too many top cells:" + topCells)

  val topCell: Sort = topCells.head
  private val sortedSorts: Seq[Sort] = tsort(edges).toSeq
  val levels: Map[Sort, Int] = edges.toList.sortWith((l, r) => sortedSorts.indexOf(l._1) < sortedSorts.indexOf(r._1)).foldLeft(Map(topCell -> 0)) {
    case (m: Map[Sort, Int], (from: Sort, to: Sort)) =>
      m + (to -> (m(from) + 1))
  }

  private lazy val mainCell = {
    val mainCells = cellProductions.filter(x => x._2.att.contains("maincell")).map(_._1)
    if (mainCells.size > 1)
      throw new AssertionError("Too many main cells:" + mainCells)
    if (mainCells.isEmpty)
      throw new AssertionError("No main cell found")
    mainCells.head
  }

  override def getCellBagSortOfCell(n: Sort): Sort = cellToBagSort(n)

  override def getLevel(k: Sort): Int = levels.getOrElse(k, -1)

  override def isParentCell(k: Sort): Boolean = edges exists { case (c, _) => c == k }

  override def getMultiplicity(k: Sort): Multiplicity =
    if (cellToBagSort.contains(k))
      Multiplicity.STAR
    else if (cellProductions(k).att.contains("unit"))
      Multiplicity.OPTIONAL
    else
      Multiplicity.ONE

  override def getParent(k: Sort): Sort = edges collectFirst { case (p, `k`) => p } get

  override def isCell(k: Sort): Boolean = cellSorts.contains(k)

  override def isCellCollection(s: Sort): Boolean = cellBagSorts.contains(s)

  override def isCellLabel(kLabel: KLabel): Boolean = cellLabelsToSorts.contains(kLabel)

  override def isLeafCell(k: Sort): Boolean = !isParentCell(k)

  override def getChildren(k: Sort): util.List[Sort] =
    cellProductions(k).items.filter(_.isInstanceOf[NonTerminal]).map(_.asInstanceOf[NonTerminal].sort).flatMap { s => {
      if (cellBagSorts(s))
        Seq(cellBagToCell(s))
      else
        Seq(s)
    }
    }.asJava

  override def leafCellType(k: Sort): Sort = cellProductions(k).items.collectFirst { case NonTerminal(n) => n } get

  override def getDefaultCell(k: Sort): KApply = cellInitializer(k)

  // This relies on initializer terms all being function calls
  override def isConstantInitializer(k: Sort): Boolean = cellInitializer(k).klist.size == 1

  override def getCellLabel(k: Sort): KLabel = cellLabels(k)

  override def getCellSort(kLabel: KLabel): Sort = cellLabelsToSorts(kLabel)

  override def getCellFragmentLabel(k: Sort): KLabel = cellFragmentLabel(k.name)

  override def isCellFragment(s: Sort): Boolean = cellFragmentToCell.contains(s)

  override def getCellOfFragment(s: Sort): Sort = cellFragmentToCell(s)

  override def getFragmentOfCell(s: Sort): Sort = cellToFragment(s)

  override def getCellAbsentLabel(k: Sort): KLabel = cellAbsentLabel(k)

  override def isCellOpt(s: Sort): Boolean = cellOptToCell.contains(s)

  override def getCellOfOpt(s: Sort): Sort = cellOptToCell(s)
  override def getCellOptOfCell(s: Sort): Sort = cellToCellOpt(s)

  override def getRootCell: Sort = topCell

  override def getComputationCell: Sort = mainCell

  override def getCellSorts: util.Set[Sort] = cellSorts.asJava

  override def getUnit(k: Sort): KApply =
    if (getMultiplicity(k) == Multiplicity.OPTIONAL)
      KApply(KLabel(cellProductions(k).att.get[String]("unit").get))
    else
      KApply(KLabel(cellBagProductions(getCellBagSortOfCell(k)).att.get[String]("unit").get))

  override def getConcat(k: Sort): KLabel =
    cellBagProductions(getCellBagSortOfCell(k)).klabel.get

  override def getCellForConcat(concat: KLabel): Option[Sort] = cellSorts
    .filter(s => cellToBagSort.contains(s)
      && cellBagProductions(getCellBagSortOfCell(s)).klabel.get.equals(concat))
    .headOption

  override def getCellForUnit(unitLabel: KLabel): Option[Sort] = {
    cellSorts
      .filter(s => cellToBagSort.contains(s)
        && KLabel(cellBagProductions(getCellBagSortOfCell(s)).att.get[String]("unit").get).equals(unitLabel))
      .headOption
  }

  override def getCellOfFragmentMember(s: Sort): Sort = {
    if (isCell(s)) {
      s
    } else if (isCellOpt(s)) {
      getCellOfOpt(s)
    } else if (cellBagSorts(s)) {
      cellBagToCell(s)
    } else {
      null
    }
  }
}
