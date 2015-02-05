package org.kframework.tiny


trait Theory {
  def normalize(k: K): K
  //  def deepNormalize(k: K): K = k match {
  //    case KApp(label, children, att) =>
  //      normalize(label(children map deepNormalize, att).normalize(this)) // normalization inside the label apply
  //    case l: KLeaf => normalize(l)
  //  }
}

object FreeTheory extends Theory {
  def normalize(k: K): K = k match {
    case EqualsMatcher(a, b) => if (a == b) True else False
    case t => t
  }
}

case class RewriteBasedTheory(rw: K => K) extends Theory {
  def normalize(k: K): K = rw(k)
}
