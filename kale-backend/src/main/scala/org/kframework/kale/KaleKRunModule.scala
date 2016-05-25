package org.kframework.kale

import java.util.function.Function

import com.google.inject.{AbstractModule, TypeLiteral}
import com.google.inject.multibindings.MapBinder
import org.kframework.definition.Module
import org.kframework.rewriter.Rewriter

class KaleKRunModule extends AbstractModule {
  override def configure(): Unit = {
    val rewriterBinder: MapBinder[String, Function[Module, org.kframework.rewriter.Rewriter]] = MapBinder.newMapBinder(binder, TypeLiteral.get(classOf[String]), new TypeLiteral[Function[Module, org.kframework.rewriter.Rewriter]]() {})
    rewriterBinder.addBinding("kale").toInstance(KaleRewriter.self.apply)
  }
}
