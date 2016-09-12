// Copyright (c) 2014-2016 K Team. All Rights Reserved.
DEF: null -> null
module TEST
  syntax Foo@TEST ::= "t"

  syntax Foo@TEST ::= "u"

  syntax Foo@TEST ::= "x"

  syntax Foo@TEST ::= "y"

  syntax Foo@TEST ::= "z"

  syntax left x

  syntax prioritiesx
> y z
> t
> u

  syntax right y


endmodule

