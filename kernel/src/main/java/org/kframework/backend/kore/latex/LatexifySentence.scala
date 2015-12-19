package org.kframework.backend.kore.latex

import org.kframework.attributes.Att
import org.kframework.definition._
import org.kframework.kore.ADT._
import org.kframework.kore.K
import org.kframework.utils.StringUtil

import scala.collection.JavaConverters._
/**
 * Created by Edgar Pek on 9/28/15.
 */
class LatexifySentence(e: String) {

  val debugPrefix = "\t\tLATEXIFY "
  var result : String = ""
  def clearResult = result = ""

  var endl = e
  def setEndl(s : String) = endl = s

  var indent : String = ""
  private def increaseIndent = indent += "  "
  private def decreaseIndent = indent = indent.substring(2)
  def setIndent(s : String) = indent = s

  private def newLine = result += (endl + indent)

  private var firstProduction : Boolean = true
  def resetFirstProduction = firstProduction = true
  private var terminalBefore : Boolean = false

  private def isOnNewLine: Boolean = {
    val lastEndl = result.lastIndexOf(endl)
    return ((lastEndl == -1 && result.length == indent.length)          //nested LatexFilter with no new lines yet
            || result.length == lastEndl + endl.length + indent.length) //top-level or nested LatexFilter with new lines
  }

  private def decreaseIndentAndNewLineIfNeeded: Unit = {
    if (isOnNewLine) {
      decreaseIndent;
      val sb = new StringBuilder(result)
      sb.delete(sb.length - 2, sb.length)
      result = sb.toString()
    } else {
      decreaseIndent
      newLine
    }
  }

  def latexify(s: Sentence) : Unit =
    s match {
      case cfg : Configuration =>
        result += "\\kconfig{"
        result += "TODO: result of latexifying config goes here"
        this.latexify(cfg.body)
        this.latexify(cfg.ensures)
        result += "}" + endl
      case ctx : Context =>
        this.latexify(ctx.body)
        this.latexify(ctx.requires)

      case mc : ModuleComment =>
        /* ModuleComment in KORE does not distinguish between various kinds of comments as
        *  KIL comments did (see Outer.java for literate comment parsing )*/
        println(debugPrefix + " ModuleComment " + mc.comment)
        result += "\\begin{kblock}[text]" + endl
        result += mc.comment
        result += endl + "\\end{kblock}" + endl
        result += "%" + endl

        // Production
      case prod : Production =>
        newLine
        if (firstProduction) {
          result += "\\syntax"
          firstProduction = false
        } else {
          result += "\\syntaxCont"
        }
        increaseIndent ; newLine
        result += "{"
        // TODO: handle pattarn instantiation see old LatexFilter @ 150 - 162
        for(pi : ProductionItem <- prod.items) {
         if(!(pi.isInstanceOf[Terminal])) {
            result += "{"
            latexify(pi)
            result += "}"
          } else {
           latexify(pi)
         }
        }
        // -------------------------------------------------------------------
        result += "}"
        newLine
        result += "{"
        // TODO: handle attributes
        result += "}"
        decreaseIndent

        //prod.items.foreach(handleProductionItem)
      case r : Rule =>
        result += "\\krule"
        // TODO: is label in att?
        //if (!"".equals(rule.getLabel())) {
        //  result.append("[" + rule.getLabel() + "]");
        //}
        result += "{"
        increaseIndent
        increaseIndent
        newLine
        this.latexify(r.body)
        decreaseIndentAndNewLineIfNeeded
        result += "}"
        newLine
        result += "{"
        if (!(r.requires.isInstanceOf[KToken] && r.requires.asInstanceOf[KToken].s.contentEquals("true")))
          this.latexify(r.requires)
        result += "}{"
        if (!(r.ensures.isInstanceOf[KToken] && r.ensures.asInstanceOf[KToken].s.contentEquals("true")))
          this.latexify(r.ensures)
        result += "}{"
        // TODO: handle attributes
        result += "}"
        result += "{"
        // if (termComment) result.append("large");
        result += "}"
        decreaseIndent
        newLine
        result += "%" + endl
        newLine
      case sa : SyntaxAssociativity =>
        //println(debugPrefix + " Syntax Associativity " + sa.assoc.toString + " tags: " + sa.tags.toString())
      case sp : SyntaxPriority =>
        //println(debugPrefix + " Syntax Priority " + sp.priorities.toString())
      case ss : SyntaxSort =>
        //println(debugPrefix + "Syntax Sort " + ss.sort.name)
      case err =>
        throw new AssertionError("latexify(sentence) error " + err.toString + " not handled!")

    }

  private def latexify(k : K) : Unit =
    k match {
      case ka @ KApply(_, _, _) =>
        println("\t\tKApply: ")
        println("\t\t klabel: " + ka.klabel)
        println("\t\t klist: " + ka.klist.items.asScala.foreach(this.latexify))
      case kr : KRewrite =>
        //wantParens.push(Boolean.TRUE);
        if (!isOnNewLine) {
          newLine
        }
        if(!isOnNewLine) {
          newLine
        }
        result += "\\reduce"
        increaseIndent
        newLine
        result += "{"
        this.latexify(kr.left)
        result += "}"
        newLine
        result += "{"
        this.latexify(kr.right)
        result += "}"
        decreaseIndent
        newLine
        //wantParens.pop();
      case kt : KToken =>
        result += "\\constant[" + StringUtil.latexify(kt.sort.name) + "]{" + StringUtil.latexify(kt.s) + "}"
      case kv : KVariable =>
        println("\t\tKVariable: " + "name: " + kv.name)
      case s: KSequence =>
        println("\t\tKSequence: " + " elements: " + s.elements.toString())
      case ikl : InjectedKLabel =>
        println("\t\tInjectedKLabel: " + " klabel: " + ikl.klabel)
      case err =>
        throw new AssertionError("latexify(k) error" + err.toString + " not handled!")
    }

  private def handleProductionItem(pi: ProductionItem) {
    newLine
    if (firstProduction) {
      result += "\\syntax"
      firstProduction = false
    } else {
      result += "\\syntaxCont"
    }
    increaseIndent ; newLine
    result += "{"
    // TODO: handle pattarn instantiation see old LatexFilter @ 150 - 162
    result += "{"
    latexify(pi)
    result += "}"
    // -------------------------------------------------------------------
    result += "}"
    newLine
    result += "{"
    // TODO: handle attributes old LatexFilter @ 170
    result += "}"
    decreaseIndent
  }

  private def latexify(pi : ProductionItem): Unit =
    pi match {
      case nt : NonTerminal =>
        result += "{\\nonTerminal{\\sort{" + StringUtil.latexify(nt.sort.name) + "}}}"
        terminalBefore = false;
      case rt : RegexTerminal =>
        println("\t\tRegex terminal " + "precedRegex: " + rt.precedeRegex + "regex:" + rt.regex + "followRegex: " + rt.followRegex)
      case t : Terminal =>
        if (terminalBefore) result += "{}"
        result += "\\terminal{" + StringUtil.latexify(t.value) + "}"
        // TODO: (Edgar Pek) handle followRegex which was not part of the old version
      case err =>
        throw new AssertionError("letexify production item error: " + err + " not supported!")
    }


}
