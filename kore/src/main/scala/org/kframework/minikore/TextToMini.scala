package org.kframework.minikore

import org.kframework.minikore.MiniKore._

// directly using BufferedReader without the wrapper of Scala's BufferedSource
object ScannerJava {
  // should be used after `init()`
  private var bufferedReader: java.io.BufferedReader = _
  var input: Iterator[Char] = _
  //
  def init(file: java.io.File): Unit = {
    bufferedReader = new java.io.BufferedReader(new java.io.FileReader(file))
    readLine()
  }
  //
  def readLine(): Unit = {
    val line = bufferedReader.readLine()
    if (line == null) throw new java.io.EOFException()
    input = line.iterator // TODO(Daejun): how efficient is it to generate iterator view of Java string?
  }
}

object ScannerScala {
  // should be used after `init()`
  private var stream: io.Source = _
  private var lines: Iterator[String] = _
  var input: Iterator[Char] = _
  //
  def init(file: java.io.File): Unit = {
    stream = io.Source.fromFile(file)
    lines = stream.getLines()
    readLine()
  }
  //
  def readLine(): Unit = {
    if (lines.hasNext) {
      input = lines.next().iterator
    } else { // end of file
      throw new java.io.EOFException()
    }
  }
}

object TextToMini {
  val Scanner = ScannerScala // ScannerJava // TODO(Daejun): compare performance against ScannerJava

  // abstract unreadable stream: next(), putback()
  var lookahead: Option[Char] = None
  //
  def next(): Char = {
    lookahead match {
      case Some(c) =>
        lookahead = None
        c
      case None =>
        if (Scanner.input.hasNext) {
          Scanner.input.next() match {
            case ' ' | '\t' => next() // skip white spaces
            case '\n' | '\r' => ??? // next() // stream.getLines implicitly drops newline characters
            case c => c
          }
        } else { // end of line
          Scanner.readLine()
          next()
        }
    }
  }
  def putback(c: Char): Unit = {
    lookahead match {
      case Some(_) => ???
      case None =>
        lookahead = Some(c)
    }
  }
  //
  def expect(str: String): Unit = {
    for (c <- str) {
      if (next() == c) ()
      else ???
    }
  }

  //////////////////////////////////////////////////////////

  def parse(file: java.io.File): Definition = {
    Scanner.init(file)
    parseDefinition()
  }

  // Definition = Attributes Modules
  def parseDefinition(): Definition = {
    val att = parseAttributes()
    val modules = parseModules(Seq())
    Definition(modules, att)
  }

  // Attributes = [ List{Pattern, ',', ']'} ]
  def parseAttributes(): Attributes = {
    expect("[")
    val att = parseList(parsePattern, ',', ']')
    expect("]")
    att
  }

  // Modules = <EOF> // <empty>
  //         | Module Modules
  def parseModules(modules: Seq[Module]): Seq[Module] = {
    val isEOF =
      try {
        putback(next()) // check if EOF
        false
      } catch {
        case _: java.io.EOFException => true
        case _: Throwable => ???
      }
    if (isEOF) modules
    else {
      val mod = parseModule()
      parseModules(modules :+ mod)
    }
  }

  // Module = module ModuleName Sentences endmodule Attributes
  def parseModule(): Module = {
    expect("module")
    val name = parseModuleName()
    val sentences = parseSentences(Seq())
    expect("endmodule")
    val att = parseAttributes()
    Module(name, sentences, att)
  }

  // Sentences = <lookahead>(e) // <empty>
  //           | Sentence Sentences
  // Sentence = imports Import
  //          | syntax Sort SortOrSymbolDeclaration
  //          | rule Rule
  //          | axiom Axiom
  // SortOrSymbolDeclaration = Attributes
  //                         | ::= SymbolDeclaration
  // Sort = Name
  def parseSentences(sentences: Seq[Sentence]): Seq[Sentence] = {
    next() match {
      case 'i' => expect("mports")
        val sen = parseImport()
        parseSentences(sentences :+ sen)
      case 's' => expect("yntax")
        val sort = parseName()
        next() match {
          case '[' => putback('[')
            val att = parseAttributes()
            val sen = SortDeclaration(sort, att)
            parseSentences(sentences :+ sen)
          case ':' => expect(":=")
            val (symbol, args, att) = parseSymbolDeclaration()
            val sen = SymbolDeclaration(sort, symbol, args, att)
            parseSentences(sentences :+ sen)
        }
      case 'r' => expect("ule")
        val sen = parseRule()
        parseSentences(sentences :+ sen)
      case 'a' => expect("xiom")
        val sen = parseAxiom()
        parseSentences(sentences :+ sen)
      case 'e' => putback('e') // endmodule
        sentences
    }
  }

  // SymbolDeclaration = Symbol ( List{Sort, ',', ')'} ) Attributes
  // Sort = Name
  def parseSymbolDeclaration(): Tuple3[String, Seq[String], Attributes] = {
    val symbol = parseSymbol()
    expect("(")
    val args = parseList(parseName, ',', ')')
    expect(")")
    val att = parseAttributes()
    (symbol, args, att)
  }

  // Import = ModuleName Attributes
  def parseImport(): Import = {
    val name = parseModuleName()
    val att = parseAttributes()
    Import(name, att)
  }

  // Rule = Pattern Attributes
  def parseRule(): Rule = {
    val pattern = parsePattern()
    val att = parseAttributes()
    Rule(pattern, att)
  }

  // Axiom = Pattern Attributes
  def parseAxiom(): Axiom = {
    val pattern = parsePattern()
    val att = parseAttributes()
    Axiom(pattern, att)
  }

  // Pattern = Variable
  //         | Symbol ( List{Pattern, ',', ')'} )
  //         | Symbol ( String )
  //         | \true ( )
  //         | \false ( )
  //         | \and ( Pattern , Pattern )
  //         | ...
  def parsePattern(): Pattern = {
    next() match {
      case c if isNameStart(c) => putback(c)
        parseVariable()
      case c if isSymbolStart(c) => putback(c)
        val symbol = parseSymbol()
        expect("(")
        next() match {
          case '"' => putback('"')
            val value = parseString()
            expect(")")
            DomainValue(symbol, value)
          case c => putback(c)
            val args = parseList(parsePattern, ',', ')')
            expect(")")
            Application(symbol, args)
        }
      case '\\' =>
        val c1 = next()
        val c2 = next()
        (c1, c2) match {
          case ('t', 'r') => expect("ue()")
            True()
          case ('f', 'a') => expect("lse()")
            False()
          case ('a', 'n') => expect("d(")
            val p1 = parsePattern(); expect(",")
            val p2 = parsePattern(); expect(")")
            And(p1, p2)
          case ('o', 'r') => expect("(")
            val p1 = parsePattern(); expect(",")
            val p2 = parsePattern(); expect(")")
            Or(p1, p2)
          case ('n', 'o') => expect("t(")
            val p = parsePattern(); expect(")")
            Not(p)
          case ('i', 'm') => expect("plies(")
            val p1 = parsePattern(); expect(",")
            val p2 = parsePattern(); expect(")")
            Implies(p1, p2)
          case ('e', 'x') => expect("ists(")
            val v = parseVariable(); expect(",")
            val p = parsePattern(); expect(")")
            Implies(v, p)
          case ('f', 'o') => expect("rall(")
            val v = parseVariable(); expect(",")
            val p = parsePattern(); expect(")")
            ForAll(v, p)
          case ('n', 'e') => expect("xt(")
            val p = parsePattern(); expect(")")
            Next(p)
          case ('r', 'e') => expect("write(")
            val p1 = parsePattern(); expect(",")
            val p2 = parsePattern(); expect(")")
            Rewrite(p1, p2)
          case ('e', 'q') => expect("ual(")
            val p1 = parsePattern(); expect(",")
            val p2 = parsePattern(); expect(")")
            Equal(p1, p2)
          case _ => ???
        }
      case _ => ???
    }
  }

  // Variable = Name : Sort
  def parseVariable(): Variable = {
    val name = parseName()
    expect(":")
    val sort = parseName()
    Variable(name, sort)
  }

  //////////////////////////////////////////////////////////

  // String = " <char> "
  def parseString(): String = {
    def loop(s: StringBuilder): String = {
      next() match {
        case '"' =>
          s.toString()
        case '\\' =>
          val c = next()
          ??? // TODO(Daejun): handle escape char
          s += c; loop(s)
        case c =>
          s += c; loop(s)
      }
    }
    next() match {
      case '"' => loop(new StringBuilder())
      case _ => ???
    }
  }

  // ModuleName = [A-Z][A-Z-]*
  def parseModuleName(): String = {
    def loop(s: StringBuilder): String = {
      next() match {
        case c if ('A' <= c && c <= 'Z') || c == '-'  =>
          s += c; loop(s)
        case c => putback(c)
          s.toString()
      }
    }
    next() match {
      case c if isModuleNameStart(c) => loop(new StringBuilder(c.toString))
      case _ => ???
    }
  }
  def isModuleNameStart(c: Char): Boolean = {
    'A' <= c && c <= 'Z'
  }

  // Symbol = [a-z#$@%^_][a-zA-Z0-9_]*
  def parseSymbol(): String = {
    def loop(s: StringBuilder): String = {
      next() match {
        case c if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9') || c == '_'  =>
          s += c; loop(s)
        case c => putback(c)
          s.toString()
      }
    }
    next() match {
      case c if isSymbolStart(c) =>
        loop(new StringBuilder(c.toString))
      case _ => ???
    }
  }
  def isSymbolStart(c: Char): Boolean = {
    ('a' <= c && c <= 'z') || c == '#' || c == '$' || c == '@' || c == '%' || c == '^' || c == '_'
  }

  // Name = [A-Z][a-zA-Z]*  // for Sort or VariableName
  def parseName(): String = {
    def loop(s: StringBuilder): String = {
      next() match {
        case c if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') =>
          s += c; loop(s)
        case c => putback(c)
          s.toString()
      }
    }
    next() match {
      case c if isNameStart(c) => loop(new StringBuilder(c.toString))
      case _ => ???
    }
  }
  def isNameStart(c: Char): Boolean = {
    'A' <= c && c <= 'Z'
  }

  // List{Elem, <sep>, <endsWith>}
  //
  // List = <endsWith> // <empty>
  //      | Elem List2
  // List2 = <endsWith> // <empty>
  //       | <sep> Elem List2
  def parseList[T](parseElem: () => T, sep: Char, endsWith: Char): Seq[T] = {
    assert(sep != endsWith)
    def parseList2(lst: Seq[T]): Seq[T] = {
      next() match {
        case c if c == endsWith => putback(c)
          lst
        case c if c == sep =>
          val elem = parseElem()
          parseList2(lst :+ elem)
        case _ => ???
      }
    }
    next() match {
      case c if c == endsWith => putback(c)
        Seq()
      case c => putback(c)
        val elem = parseElem()
        parseList2(Seq(elem))
    }
  }

  def test(file: java.io.File): Unit = {
    Scanner.init(file)
    println("-------------------------")
    val now = java.lang.System.nanoTime();
    try {
      println(next())
      println(next())
      println(next())
      println(next())
      println(next())
      println(next())
      println(next())
    } catch {
      case _: java.io.EOFException => println("end of file")
      case _: Throwable => ???
    }
    println(java.lang.System.nanoTime() - now);
    println("-------------------------")
  }

}
