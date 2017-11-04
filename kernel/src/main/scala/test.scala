import org.kframework.definition.test
import org.kframework.attributes.Source
import org.kframework.parser.concrete2kore.ParseInModule

class test2 {
  val myParser = new ParseInModule(test.EXP)
  val parseResult = myParser.parseString("0 + 0", test.Exp, Source(""))
}
