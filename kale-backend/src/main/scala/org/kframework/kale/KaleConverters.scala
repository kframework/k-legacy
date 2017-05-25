package org.kframework.kale

import org.kframework.builtin.Sorts
import org.kframework.definition.Module
import org.kframework.frontend
import org.kframework.frontend._

import scala.collection.Map

class KaleConverters(m: Module)(implicit val env: Environment) {

//  import env._
//  import env.builtin._
//
//  private val emptyKSeq = FreeLabel0(".K")(env)
//  private val kseq = new AssocWithIdListLabel("~>", emptyKSeq())
//
//  val klabelToLabelRename = Map(
//    "keys" -> "_Map_.keys",
//    "lookup" -> "_Map_.lookup",
//    "Set:in" -> "_Set_.in",
//    "Map:lookup" -> "_Map_.lookup"
//  )
//
//  val labelToKLabelRename = klabelToLabelRename.map(p => (p._2, p._1)).toMap
//
//  def renames(labelName: String) = klabelToLabelRename.getOrElse(labelName, labelName)
//
//  def kSortOf(t: Term): Option[frontend.Sort] =
//    if (!t.isGround)
//      None
//    else
//      m.sortFor
//        .get(KORE.KLabel(t.label.name))
//        .orElse(
//          t.label match {
//            case tokenLabel: GENERIC_TOKEN => Some(m.resolve(frontend.KORE.Sort(tokenLabel.sort.name)))
//            case BOOLEAN => Some(m.resolve(Sorts.Bool))
//            case INT =>
//              Some(m.resolve(Sorts.Int))
//            case STRING => Some(m.resolve(Sorts.String))
//            case `kseq` => Some(m.Sort("KItem"))
//            case `emptyKSeq` => Some(m.Sort("K"))
//            // TODO: handle sorting for conjunctions
//            //            case And =>
//            //              val nonFormulas = And.asSubstitutionAndTerms(t)._2.filter(!_.label.isInstanceOf[FormulaLabel])
//            //              if (nonFormulas.size == 1)
//            //                kSortOf(nonFormulas.head)
//            //              else
//            //                ??? // we have more than one non-formula term. computer the least sort?
//            case Variable => None
//          })
//
//  private lazy val uninterpretedTokenLabels: Map[Sort, ConstantLabel[String]] = (labels collect {
//    case l@GENERIC_TOKEN(s) => (s, l)
//  }).toMap + (Sort("KConfigVar@BASIC-K") -> GENERIC_TOKEN(Sort("KConfigVar@BASIC-K")))
//
//  def convert(klabel: KLabel): Label = label(klabelToLabelRename.getOrElse(klabel.name, klabel.name))
//
//  def convert(body: K): Term = body match {
//    case Unapply.KToken(s, sort) => sort match {
//      case Sorts.Bool => BOOLEAN(s.toBoolean)
//      case Sorts.Int => INT(s.toInt)
//      case Sorts.String => STRING(s)
//      case _ => uninterpretedTokenLabels(Sort(sort.name))(s)
//    }
//    case Unapply.KApply(klabel, list) if klabel.name == "#KSequence" => kseq(list map convert)
//    case Unapply.KApply(klabel, list) => convert(klabel).asInstanceOf[NodeLabel](list map convert)
//    case v: KVariable => Variable(v.name)
//    //      val kaleV = Variable(v.name)
//    //      v.att.get(Att.sort)
//    //        .map(sort => env.And(
//    //          kaleV,
//    //          Equality(env.label("is" + sort.name).asInstanceOf[Label1](kaleV), BOOLEAN(true)))
//    //        )
//    //        .getOrElse(kaleV)
//    case r: KRewrite => Rewrite(convert(r.left), convert(r.right))
//  }
//
//  def convertBack(l: Label): KLabel = KORE.KLabel(labelToKLabelRename.getOrElse(l.name, l.name))
//
//  def convertBack(term: Term): K = term match {
//    case Variable(x) => KORE.KVariable(x)
//    case emptyKSeq() => KORE.KSequence()
//    case t@Node(`kseq`, _) => KORE.KSequence(kseq.asList(t).toList map convertBack: _*)
//    case Node(label, subterms) => KORE.KApply(convertBack(label), (subterms map convertBack).toSeq: _*)
//    case t@Constant(label, value) => KORE.KToken(value.toString, kSortOf(t).get)
//  }
}
