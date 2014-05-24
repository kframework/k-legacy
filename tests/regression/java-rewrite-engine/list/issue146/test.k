// Copyright (c) 2014 K Team. All Rights Reserved.

module TEST
  imports LIST

  configuration <T> <in> MyListItem(2), MyListItem(1) </in> <k> $PGM:K </k> </T>

  syntax KItem ::= "test"

  rule <in> MyListItem(1), V:MyList, MyListItem(1) => .MyList </in>

endmodule
