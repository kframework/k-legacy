package org.kframework

import org.kframework.kore.K

/**
 * Created by dwightguth on 5/8/15.
 */
trait Strategy[T] {

  def execute(k: K, rewriter: Rewriter): T
}
