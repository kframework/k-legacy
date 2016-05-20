package org.kframework.kale

import java.util
import collection.JavaConversions._

import com.google.inject.Module
import org.kframework.main.KModule

class KaleKModule extends KModule {
  override def getKDocModules: util.List[Module] = List()

  override def getKompileModules: util.List[Module] = List(new KaleKompileModule())

  override def getKastModules: util.List[Module] = List()

  override def getKRunModules(definitionSpecificModules: util.List[Module]): util.List[Module] = List()

  override def getDefinitionSpecificKRunModules: util.List[Module] = List(new KaleKRunModule())

  override def getKTestModules: util.List[Module] = List()
}
