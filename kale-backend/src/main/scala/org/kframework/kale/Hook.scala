package org.kframework.kale

object Hook {
  def apply(hookName: String, labelName: String)(implicit env: Environment): Option[Label] = {
    import env.builtin.{INT, BOOLEAN}
    Some(hookName) collect {
      case "INT.le" => Operator(labelName, INT, BOOLEAN, (a: Int, b: Int) => a <= b)
      case "INT.add" => PrimitiveFunction2(labelName, INT, (a: Int, b: Int) => a + b)
      case "INT.tdiv" => PrimitiveFunction2(labelName, INT, (a: Int, b: Int) => a / b)
      case "INT.ne" => Operator(labelName, INT, BOOLEAN, (a: Int, b: Int) => a != b)
      case "BOOL.and" => PrimitiveFunction2[Boolean](labelName, BOOLEAN, _ && _)
      case "BOOL.or" => PrimitiveFunction2[Boolean](labelName, BOOLEAN, _ || _)
      case "BOOL.not" => PrimitiveFunction1[Boolean](labelName, BOOLEAN, !_)
      case "SET.unit" => assert(labelName == ".Set"); env.builtin.BuiltinSetUnit
      case "SET.concat" => assert(labelName == "_Set_"); env.builtin.BuiltinSet
    }
  }

  def relativeHook(relativeHook: String)(implicit env: Environment): Option[Label] = relativeHook.split('.').toSeq match {
    case Seq(baseLabel: String, hookName: String) => relativeHookByBaseLabel(baseLabel, hookName)
    case _ => None
  }

  def relativeHookByBaseLabel(baseLabel: String, hookName: String)(implicit env: Environment): Option[Label] = env.label(baseLabel) match {
    case l: env.builtin.MapLabel => hookName match {
      case "lookup" => Some(l.lookup)
      case "keys" => Some(l.keys)
      case _ => None
    }
    case s: env.builtin.SetLabel => hookName match {
      case "in" => Some(s.in)
    }
    case _ => None
  }
}
