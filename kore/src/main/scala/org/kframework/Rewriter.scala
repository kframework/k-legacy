package org.kframework

import org.kframework.definition.{Rule, Module}
import org.kframework.kore.{KVariable, K}

trait RewriterConstructor extends (Module => Rewriter)

trait Rewriter {
  //  def normalize(k: K): K
  //  def substitute(k: K, s: KVariable => K): K

  //  def step(k: K): K

  /**
   * (disregard this javadoc comment for now)
   * Takes one rewriting step.
   * - for regular execution, it returns the next K or False (i.e. an empty Or)
   * - for symbolic execution, it can return any formula with symbolic constraints
   * - for search, it returns an Or with multiple ground terms as children
   */

  /**
   * Converts a term into a K which has been optimized for execution in the backend.
   * @param k A K term to convert
   * @return A K term which, when passed to other methods in this interface, does not require further conversion.
   */
  def convert(k: K): K = k

  /**
   * Matches a particular subject against a pattern
   * @param k The subject to match against.
   * @param trace true if trace information about the matching should be printed to standard output.
   * @param rule The rule to match and check the side condition of.
   * @throws UnsupportedOperationException if the class does not support rule traces, and trace is true.
   * @return A list of substitutions satisfying the rule.
   */
  def `match`(k: K, trace: Boolean, rule: Rule): java.util.List[_ <: java.util.Map[_ <: KVariable, _ <: K]]

  /**
   * Substitutes a particular term using the right hand side of a rule.
   * @param substitution A mapping of substitutions for variables in the rule.
   * @param rule The rule whose right hand side should be used for the substitution
   * @return the right hand side of rule substituted using the substitution.
   */
  def substitute(substitution: java.util.Map[_ <: KVariable, _ <: K], rule: Rule): K

  def execute(k: K): K

  /**
   * Returns the list of rules in the definition being rewritten over, in the order they should be tried.
   * These rules can be passed to other methods in this interface without further conversion.
   */
  def rules : java.util.List[_ <: Rule]
}