package org.kframework.backend.kore.latex

import org.kframework.attributes.{Att, Location}
import org.kframework.definition._
import org.kframework.kore.{KApply, K, Sort}
import org.kframework.utils.StringUtil
import org.kframework.utils.file.JarInfo
import collection._

/**
 * Created by Edgar Pek on 10/15/15.
 */
class LatexifyDefinition(d: Definition, ind: String = "") {

  /// the body (result) and preamble of "latexification"
  var result: StringBuilder = new StringBuilder
  var preamble: StringBuilder = new StringBuilder

  // helper strings
  private var indent: String = ind
  private val endl: String = System.getProperty("line.separator")

  // flags
  private var hasTitle: Boolean = false
  private var terminalBefore: Boolean = false

  private var colors: Map[String, String] = Map.empty[String, String]
  private val locAtts = List("Location", "org.kframework.attributes.Location")

  private var latexPatterns: LatexPatterns = new LatexPatterns(d)

  object CommentTypes extends Enumeration {
    type CommentTypes = Value
    val LATEX, PREAMBLE, COMMON = Value
  }
  import CommentTypes._

  private def newLine() = result.append(endl).append(indent)
  private def increaseIndent() = indent += "  "
  private def decreaseIndent() = indent = indent.substring(2)

  def latexifyDefinition(): Unit = {
    latexPatterns.createPatterns
    result.append("\\begin{kdefinition}" + endl + "\\maketitle" + endl)
    handleDefinition()
    result.append("\\end{kdefinition}" + endl)
    if (!hasTitle) {
      preamble.append("\\title{" + d.mainModule.name + "}" + endl)
      hasTitle = true
    }
  }

  // sort comments and modules at the top level, sort sentences inside the modules
  private def handleDefinition() = {
    val modules = sortedModules(d.modules)

    traverseByLocation(modules)

    println("Done with the outer traversing...")

  }

  private def traverseByLocation(modules: List[Module]) : Unit = ???
//    (modules) match {
//      case (m :: ms) =>
//        val commentLoc: Location = ???
//        val moduleLoc  = getLocationAtt(m.att)
//        if (commentLoc.compareTo(moduleLoc) < 0) {
//          latexifyDefinitionComment(c)
//          traverseByLocation(cs, modules)
//        } else {
//          latexifyDefinitionModule(m)
//          traverseByLocation(comments, ms)
//        }
//      case (c :: cs, Nil) =>
//        latexifyDefinitionComment(c)
//        traverseByLocation(cs, modules)
//      case (Nil, m :: ms) =>
//        latexifyDefinitionModule(m)
//        traverseByLocation(comments, ms)
//      case (Nil, Nil) =>
//    }

  private def latexifyDefinitionModule(module: Module): Unit = {
    println("[DBG] handling current module: " + module.name)
    result.append("\\begin{module}{\\moduleName{" + StringUtil.latexify(module.name) + "}}" + endl)
    // get sorted local sentences
    val sentences = sortedSentences(module.localSentences)
    // traverse local sentences
    val prodSort: Option[Sort] =
      sentences.foldLeft(None: Option[Sort])((optSort, s) => latexifySentence(s, module, optSort))

    closeProduction(prodSort)
    result.append("\\end{module}" + endl)
    println("[DBG] done with : " + module.name)

  }

  private def latexifySentence(s: Sentence, m: Module, optSort: Option[Sort]): Option[Sort] =
    s match {
      case ModuleComment(comment: String, att: Att) =>
        closeProduction(optSort); latexifyModuleComment(comment) ; None
      case prod @ Production(sort: Sort, items: Seq[ProductionItem], att: Att) =>
        latexifyProduction(prod, optSort) ; Some(sort)
      case Configuration(body: K, ensures: K, att: Att) =>
        closeProduction(optSort); latexifyConfiguration(body, ensures) ; None
      case Rule(body: K, requires: K, ensures: K, att: Att) =>
        closeProduction(optSort); latexifyRule(body, requires, ensures, m) ; None
      case err => throw new AssertionError("sentence type not supported: " + err.toString)
    }

  private def closeProduction(optSort: Option[Sort]): Unit =
    if (optSort.isDefined) {
      result.append(endl + "\\end{syntaxBlock}")
      decreaseIndent()
      newLine()
      result.append("%")
      newLine()
    }

  private def latexifyRule(body: K, requires: K, ensures: K, module: Module) = ???

  private def latexifyConfiguration(body: K, ensures: K): Unit = ???

  private def latexifyProduction(prod: Production, optSort: Option[Sort]): Unit = {
    val Production(sort: Sort, items: Seq[ProductionItem], att: Att) = prod
    if (optSort.isEmpty) {
      result.append(endl + "\\begin{syntaxBlock}")
      increaseIndent()
      result.append("\\syntax")
    } else {
      result.append("\\syntaxCont")
    }

    increaseIndent()
    newLine()

    result.append("{")
    if (att.contains("userList")) {
      latexifyUserList(sort, items)
    } else if (latexPatterns.patterns.contains(prod)) {
      val pattern: String = latexPatterns.patterns.get(prod).get
      val (_, newPattern: String) = items.foldLeft( (1, pattern) ) ( (posPat, pi) => createPattern(posPat, pi))
      result.append(newPattern)
    } else {
      items.foreach(latexifyProductionItem)
    }
    result.append("}")

    newLine()

    result.append("{")
    latexifyAttributes(att: Att)
    result.append("}")

    decreaseIndent()
  }

  private def createPattern(posPat : (Int, String) , pi: ProductionItem): (Int, String) = {
    val (pos: Int, pat: String) = posPat
    val newPat = pat.replace("{#" + pos + "}", "{" + createLatexNonTerminal(pi) + "}")
    (pos + 1, newPat)
  }

  private def latexifyAttributes(att: Att): Unit =
    att.attMap.foldLeft(true)((firstAttribute, entry) => latexifyAttribute(firstAttribute, entry))

  private def latexifyAttribute(firstAttribute: Boolean, entry: (String, KApply)): Boolean = {
    val (key: String, value: KApply) = entry
    if (key.contentEquals("latex") || key.contentEquals("html")) {
      firstAttribute
    } else {
      if (!firstAttribute) result.append(", ")

      result.append("\\kattribute{" + StringUtil.latexify(key) + "}")
      val strVal = value.toString
      if (!strVal.isEmpty) {
        result.append("(" + StringUtil.latexify(strVal) + ")")
      }

      val ret: Boolean = if (firstAttribute) !firstAttribute else firstAttribute
      ret
    }
  }

  private def createLatexNonTerminal(pi: ProductionItem): String = ???

  private def latexifyProductionItem(pi: ProductionItem): Unit = ???

  private def latexifyUserList(sort: Sort, items: Seq[ProductionItem]): Unit = ???

  private def latexifyModuleComment(c: String): Unit = {
    result.append("\\begin{kblock}[text]" + endl)
    result.append(c)
    result.append(endl + "\\end{kblock}" + endl)
    result.append("%")
    newLine()
  }

  private def latexifyDefinitionComment: Unit = {
//    val ct = getCommentTypeAtt(comment.att)
//    if (ct == CommentTypes.LATEX) {
//      result.append("\\begin{kblock}[text]" + endl)
//      result.append(comment.comment)
//      result.append(endl + "\\end{kblock}" + endl)
//      result.append("%")
//      newLine()
//    } else if (ct == CommentTypes.PREAMBLE) {
//      preamble.append(comment.comment)
//      if (comment.comment.contains("\\title{")) {
//        hasTitle = true
//      }
//    }
    ???
  }

  private def sortedSentences(ss: Set[Sentence]): List[Sentence] = {
    ss.toList.filter(s => locAtts.exists(la => s.att.attMap.contains(la)))
             .sortBy(s => getLocationAtt(s.att))
  }

  private def sortedModules(m: Set[Module]): List[Module] = {
    m.toList.filter(m => locAtts.exists(la => m.att.attMap.contains(la))
                         && m.att.attMap.contains("Source"))
            .filter(m => !sourceInIncludeDir(m.att))
            .filter(_.localSentences.nonEmpty)
            .sortBy(m => getLocationAtt(m.att))
  }

  private def sourceInIncludeDir(att : Att): Boolean = {
    val srcOpt: Option[String] = att.get("Source")
    assert(srcOpt.isDefined)
    val src: String = srcOpt.get
    val kInclude: String = JarInfo.getKIncludeDir.toString
    src.startsWith(kInclude)
  }

  private def getLocationAtt(att: Att): Location = {
    val locStringOpt: Option[String] = locAtts.find(la => att.attMap.contains(la))
    assert(locStringOpt.isDefined)
    att.get[Location](locStringOpt.get).getOrElse(throw new AssertionError("Location attribute not found."))
  }

  private def getCommentTypeAtt(att: Att): CommentTypes = {
    val commentTypeOpt: Option[String] = att.get("commenttype")
    assert(commentTypeOpt.isDefined)
    val commentType: String = commentTypeOpt.get
    if (commentType.contentEquals("")) CommentTypes.COMMON
    else if (commentType.contentEquals("!")) CommentTypes.PREAMBLE
    else if (commentType.contentEquals("@")) CommentTypes.LATEX
    else throw new AssertionError("comment type: " + commentType + " not supported!")
  }

}
