package org.kframework.kale

import com.google.inject.AbstractModule
import com.google.inject.multibindings.MapBinder
import org.kframework.kore.compile.Backend

class KaleKompileModule extends AbstractModule {
  override def configure(): Unit = {
    val kaleBackendBinder: MapBinder[String, Backend] = MapBinder.newMapBinder(binder, classOf[String], classOf[Backend])
    kaleBackendBinder.addBinding("kale").to(classOf[KaleBackend])
  }
}
