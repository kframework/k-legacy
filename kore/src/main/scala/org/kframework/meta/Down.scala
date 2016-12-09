package org.kframework.meta

import org.kframework.attributes._
import org.kframework.builtin.Sorts
import org.kframework.kore.{K, Unapply}
import org.kframework.kore.Unapply._
import org.kframework.kore.KORE.Sort

import scala.util.Try
import collection._

case class Down(imports: Set[String]) extends (K => Any) {

  import org.kframework.builtin.Sorts.KString
  import org.kframework.builtin.Sorts.String
  import org.kframework.builtin.Sorts.Int

  val AttVal = Sort("AttributeValue")

  def apply(o: K): Any = o match {
    case KToken(v, `KString`) => v
    case KToken(v, `String`) => v
    case KToken(v, `Int`) => v.toInt
    case KToken(v, `AttVal`) => v
    //    case KApply(KLabel("List"), ks, att) => ks.delegate map apply
    //    case KApply(KLabel("Seq"), ks, att) => ks.delegate map apply
    //    case KApply(KLabel("Set"), ks, att) => ks.delegate map apply toSet
    //        case t@KApply(KLabel(l), ks) if t.att.contains(Att.ClassFromUp) =>
    //          val classNameRecoveredFromUp = t.att.get[String](Att.ClassFromUp).get
    //          Reflection.construct(classNameRecoveredFromUp, ks map {
    //            apply _
    //          })
    case KApply(KLabel("org.kframework.attributes.Location"), List(KToken(startLine, Sorts.Int), KToken(startColumn, Sorts.Int), KToken(endLine, Sorts.Int), KToken(endColumn, Sorts.Int))) =>
      org.kframework.attributes.Location(startLine.toInt, startColumn.toInt, endLine.toInt, endColumn.toInt)

    case KApply(KLabel("org.kframework.attributes.Source"), List(KToken(source, Sorts.KString))) =>
      org.kframework.attributes.Source(source)

    case KApply(KLabel("org.kframework.definition.Production"), List(sort, items, atts)) =>
      org.kframework.definition.Production(
        apply(sort).asInstanceOf[org.kframework.kore.Sort],
        apply(items).asInstanceOf[Seq[org.kframework.definition.ProductionItem]],
        apply(atts).asInstanceOf[Att])

    case KApply(KLabel("org.kframework.kore.ADT$Sort"), List(name, module)) =>
      org.kframework.kore.ADT.Sort(
        apply(name).asInstanceOf[String],
        apply(module).asInstanceOf[org.kframework.definition.ModuleName])

    case KApply(KLabel("org.kframework.definition.ModuleName"), List(name)) =>
      org.kframework.definition.ModuleName(apply(name).asInstanceOf[String])

    case KApply(KLabel("org.kframework.definition.NonTerminal"), List(sort)) =>
      org.kframework.definition.NonTerminal(apply(sort).asInstanceOf[org.kframework.kore.Sort])

    case KApply(KLabel("org.kframework.definition.Terminal"), List(name, empty)) =>
      org.kframework.definition.Terminal(apply(name).asInstanceOf[String])

    case Unapply.KApply(Unapply.KLabel("Set"), l) => (l map apply).toSet
    case Unapply.KApply(Unapply.KLabel("List"), l) => l map apply
    case Unapply.KApply(Unapply.KLabel("Att"), l) => Att(l: _*)

    //    case KApply(KLabel(l), ks) =>
    //      val children = ks map {
    //        apply _
    //      }
    //
    //      val namespacesToTry = imports
    //      val matchingClasses = imports map {
    //        _ + "." + l
    //      }
    //
    //      matchingClasses
    //        .view
    //        .flatMap { name => Try(Reflection.findObject(name)).toOption }
    //        .flatMap { o => Try(Reflection.invokeMethod(o, "apply", Seq(children))).toOption }
    //        .headOption.getOrElse {
    //        matchingClasses
    //          .view
    //          .flatMap { className => Try(Reflection.construct(className, ks map apply)).toOption }
    //          .headOption
    //          .getOrElse {
    //          throw new AssertionError("Could not find a proper constructor for " + l +
    //            "\n with arguments (" + children.mkString(",") +
    //            ")\n of types (" + children.map(_.getClass()).mkString(",") +
    //            ")\n Tried:\n    " +
    //            matchingClasses.mkString("\n    "))
    //        }
    //      }
    //    //    case _ => throw new AssertionError("Could not down.")
    //  }
  }
}
