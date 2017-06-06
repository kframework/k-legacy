package org.kframework.backend.skala

import java.util.Optional

import org.kframework.definition._
import org.kframework.frontend._
import org.kframework.kale.builtin.MapImplementation
import org.kframework.{RewriterResult, kore}
import org.kframework.kore.Pattern
import org.kframework.kore.extended.implicits._
import org.kframework.minikore.converters.{KoreToMini, MiniToKore}
import org.kframework.rewriter.SearchType



class SkalaRewriter(mainModule: Module, koreDefinition: kore.Definition) extends org.kframework.rewriter.Rewriter {

  import org.kframework.kore.implementation.{DefaultBuilders => db}

  val mainKoreModule = koreDefinition.modulesMap(db.ModuleName(mainModule.name))

  val skalaBackend = SkalaBackend(koreDefinition, mainKoreModule)

  val frontendToKore = KoreToMini

  val koreToFrontend = MiniToKore


  //Todo: Needed only temporarily until we have pretty printers over Kore.
  private def filterForPrettyPrinting(p: Pattern): Pattern = p match {
    case kore.Application(kore.Symbol(".K"), _) => db.Application(KoreToMini.iKSeqNil, Seq())
    case kore.Application(symbol , args) => db.Application(symbol, args.map(filterForPrettyPrinting))
    case p:MapImplementation => db.Application(db.Symbol(p.label.name), p.children.map(filterForPrettyPrinting).toSeq)
    case p@_ => p
  }

  override def execute(k: K, depth: Optional[Integer]): RewriterResult = {
    val p: Pattern = frontendToKore(k)
    var res = p
    if(depth.isPresent()) {
      res = skalaBackend.step(p, depth.get)
    } else {
      res = skalaBackend.execute(p)
    }

    new RewriterResult(depth, koreToFrontend(filterForPrettyPrinting(res)))
  }

  override def `match`(k: K, rule: Rule): K = ???

  override def search(initialConfiguration: K, depth: Optional[Integer], bound: Optional[Integer], pattern: Rule, searchType: SearchType, resultsAsSubstitution: Boolean): K = ???

  override def executeAndMatch(k: K, depth: Optional[Integer], rule: Rule): (RewriterResult, K) = ???

  override def prove(rules: java.util.List[Rule]): java.util.List[K] = ???
}


