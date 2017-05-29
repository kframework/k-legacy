package org.kframework.backend.skala

import java.util.Optional

import org.kframework.definition._
import org.kframework.frontend._
import org.kframework.{RewriterResult, kore}
import org.kframework.kore.Pattern
import org.kframework.kore.extended.implicits._
import org.kframework.kore.implementation.{DefaultBuilders => db}
import org.kframework.minikore.converters.{KoreToMini, MiniToKore}
import org.kframework.rewriter.SearchType



class SkalaRewriter(mainModule: Module, koreDefinition: kore.Definition) extends org.kframework.rewriter.Rewriter {

  val mainKoreModule = koreDefinition.modulesMap(db.ModuleName(mainModule.name))

  val skalaBackend = SkalaBackend(koreDefinition, mainKoreModule)

  val frontendToKore = KoreToMini

  val koreToFrontend = MiniToKore

  override def execute(k: K, depth: Optional[Integer]): RewriterResult = {
    val koreK = frontendToKore(k)

    val result: Pattern = skalaBackend.step(koreK)

    new RewriterResult(depth, koreToFrontend(result))
  }

  override def `match`(k: K, rule: Rule): K = ???

  override def search(initialConfiguration: K, depth: Optional[Integer], bound: Optional[Integer], pattern: Rule, searchType: SearchType, resultsAsSubstitution: Boolean): K = ???

  override def executeAndMatch(k: K, depth: Optional[Integer], rule: Rule): (RewriterResult, K) = ???

  override def prove(rules: java.util.List[Rule]): java.util.List[K] = ???
}


