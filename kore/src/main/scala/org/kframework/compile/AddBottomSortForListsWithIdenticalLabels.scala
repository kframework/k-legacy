package org.kframework.compile

import org.kframework.attributes.Att
import org.kframework.definition._
import org.kframework.kore.ADT.Sort

import collection._

object AddBottomSortForListsWithIdenticalLabels extends BasicModuleTransformer {
  val singleton = this

  override def process(m: Module, alreadyProcessedImports: Set[Module]) = {
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
              .+(Production(newBottomSort,
                Seq(Terminal(".GeneratedListBottom")),
                Att.generatedByAtt(this.getClass) + (Production.kLabelAttribute -> userListInfo.head.pTerminator.klabel.get.name)))
          } else {
            Set()
          }
      }

    if (theAdditionalSubsortingProductions.nonEmpty)
      m.copy(unresolvedLocalSentences = m.localSentences ++ theAdditionalSubsortingProductions, imports = alreadyProcessedImports)
    else
      m
  }
}
