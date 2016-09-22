package org.kframework.compile

import org.kframework.attributes.Att
import org.kframework.definition.{Production, _}
import org.kframework.kore.ADT.Sort

import collection._

object AddBottomSortForListsWithIdenticalLabels extends (Module => Module) {
  val singleton = this

  def apply(m: Module): Module = {
    val theAdditionalSubsortingProductions = UserList.apply(m.sentences)
      .groupBy(l => l.klabel)
      .flatMap {
        case (klabel, userListInfo) =>
          val minimalSorts = m.subsorts.minimal(userListInfo map { li => li.sort })
          if (minimalSorts.size > 1) {
            val newBottomSort = Sort("GeneratedListBottom{" + klabel + "}", ModuleName(m.name))

            Set[Sentence]()
              .|(minimalSorts.map(s => Production(s, Seq(NonTerminal(newBottomSort)), Att.generatedByAtt(this.getClass))))
              .+(SyntaxSort(newBottomSort, Att.generatedByAtt(this.getClass)))
              .+(Production(newBottomSort, userListInfo.head.pTerminator.items, Att.generatedByAtt(this.getClass)))
          } else {
            Set()
          }
      }

    if (theAdditionalSubsortingProductions.nonEmpty)
      m.copy(unresolvedLocalSentences = m.localSentences ++ theAdditionalSubsortingProductions)
    else
      m
  }
}
