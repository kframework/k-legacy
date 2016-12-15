package org.kframework.meta

import org.kframework.attributes._
import org.kframework.builtin.Sorts
import org.kframework.kore.{K, KLabel, Unapply}
import org.kframework.kore.Unapply._
import org.kframework.kore.KORE.Sort

import scala.util.Try
import collection._

object Down {
  def constructorBased(imports: Set[String]): (String => Seq[AnyRef] => Any) = {
    {
      (className: String) =>
        def theClass(className: String): Option[Class[_]] = {
          val allQualifiedClassNames: Set[String] = (imports map (_ + "." + className)) + className
          val posibilities = allQualifiedClassNames flatMap { qualifiedClassName: String =>
            try {
              Some(Class.forName(qualifiedClassName))
            } catch {
              case _: ClassNotFoundException => None
            }
          }
          if (posibilities.size == 1) {
            posibilities.headOption
          } else {
            if (posibilities.isEmpty) {
              None
            } else {
              throw new AssertionError("Too many possibilities: " + posibilities)
            }
          }
        }

        (args: Seq[AnyRef]) =>
          theClass(className + "$") map { objClass =>
            val theObj = objClass.getField("MODULE$").get(null)
            val varArgMethodCall = objClass.getMethods.find(m => m.getName == "apply" && (m.getParameterTypes.toSeq match {
              case Seq(t) => t.toString.contains("Seq")
              case _ => false
            })).map(_.invoke(theObj, args))

            varArgMethodCall
              .orElse(objClass.getMethods.find(m => m.getName == "apply" && m.getParameterCount == args.size).map(_.invoke(theObj, args: _*)))
              .getOrElse(throw new AssertionError("Could not find apply method for " + className + " object."))
          } getOrElse {
            (theClass(className) map { cls =>
              val constructors = cls.getConstructors.find(_.getParameterCount == args.size)
              constructors.map(_.newInstance(args: _*)).head
            }).headOption getOrElse {
              throw new AssertionError("Could not find a constructor for " + className)
            }
          }
    }
  }
}

case class Down(hooks: String => Seq[AnyRef] => Any) extends (K => Any) {

  import org.kframework.builtin.Sorts.KString
  import org.kframework.builtin.Sorts.String
  import org.kframework.builtin.Sorts.Int

  val AttVal = Sort("AttributeValue")

  def apply(o: K): Any = o match {
    case KToken(v, `KString`) => v
    case KToken(v, `String`) => v
    case KToken(v, `Int`) => v.toInt
    case KToken(v, `AttVal`) => v
    case KApply(KLabel("Att"), atts) => Att(atts: _*)

    case KApply(KLabel(name), list) =>
      val downedList = list map apply
      val res = hooks(name)(downedList.asInstanceOf[Seq[AnyRef]])
      res
  }
}
