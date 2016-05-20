package org.kframework.kale

import java.util
import java.util.Optional

import collection.JavaConverters._

import org.kframework.RewriterResult
import org.kframework.definition.{Module, Rule}
import org.kframework.kore.{K, KVariable}
import org.kframework.rewriter.{Rewriter, SearchType}

object KaleRewriter {
  val self = this
  def apply(m: Module): KaleRewriter = new KaleRewriter(m)
}

class KaleRewriter(m: Module) extends Rewriter {
  override def execute(k: K, depth: Optional[Integer]): RewriterResult = {
    new RewriterResult(Optional.of(0), k)
  }

  override def `match`(k: K, rule: Rule): util.List[_ <: util.Map[_ <: KVariable, _ <: K]] = ???

  override def search(initialConfiguration: K, depth: Optional[Integer], bound: Optional[Integer], pattern: Rule, searchType: SearchType): util.List[_ <: util.Map[_ <: KVariable, _ <: K]] = ???

  override def executeAndMatch(k: K, depth: Optional[Integer], rule: Rule): (RewriterResult, util.List[_ <: util.Map[_ <: KVariable, _ <: K]]) = ???

  override def prove(rules: util.List[Rule]): util.List[K] = ???
}
