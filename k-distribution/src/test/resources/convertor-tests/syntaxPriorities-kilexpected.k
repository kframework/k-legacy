// Copyright (c) 2014-2015 K Team. All Rights Reserved.
DEF: null -> null
module TEST
  syntax Foo@TEST ::= "m"

  syntax Foo@TEST ::= "t"

  syntax Foo@TEST ::= "u"

  syntax Foo@TEST ::= "x"

  syntax Foo@TEST ::= "y"

  syntax Foo@TEST ::= "z"

  syntax left t m

  syntax prioritiesx
> y z
> t m
> u

  syntax right u


endmodule
