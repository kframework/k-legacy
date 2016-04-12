// Copyright (c) 2016 K Team. All Rights Reserved.
module TEST [Location(#token("3",Int@INT-SYNTAX),#token("1",Int@INT-SYNTAX),#token("13",Int@INT-SYNTAX),#token("9",Int@INT-SYNTAX)) Source(#token("src/test/resources/convertor-tests/syntaxWithPriorities.k",KString@KSTRING))]
  syntax priority x > y z > t > u
  syntax associativity right y
  syntax associativity left x
  syntax Foo@TEST ::= "z" [Location(#token("6",Int@INT-SYNTAX),#token("18",Int@INT-SYNTAX),#token("6",Int@INT-SYNTAX),#token("20",Int@INT-SYNTAX)) Source(#token("src/test/resources/convertor-tests/syntaxWithPriorities.k",KString@KSTRING)) klabel(#token("z",KString@KSTRING))]
  syntax Foo@TEST ::= "y" [Location(#token("5",Int@INT-SYNTAX),#token("18",Int@INT-SYNTAX),#token("5",Int@INT-SYNTAX),#token("40",Int@INT-SYNTAX)) Source(#token("src/test/resources/convertor-tests/syntaxWithPriorities.k",KString@KSTRING)) klabel(#token("y",KString@KSTRING)) someattribute(#token("",AttributeValue))]
  syntax Foo@TEST ::= "x" [Location(#token("4",Int@INT-SYNTAX),#token("18",Int@INT-SYNTAX),#token("4",Int@INT-SYNTAX),#token("20",Int@INT-SYNTAX)) Source(#token("src/test/resources/convertor-tests/syntaxWithPriorities.k",KString@KSTRING)) klabel(#token("x",KString@KSTRING))]
  syntax Foo@TEST ::= "u" [Location(#token("8",Int@INT-SYNTAX),#token("18",Int@INT-SYNTAX),#token("8",Int@INT-SYNTAX),#token("29",Int@INT-SYNTAX)) Source(#token("src/test/resources/convertor-tests/syntaxWithPriorities.k",KString@KSTRING)) klabel(#token("u",KString@KSTRING)) off(#token("",AttributeValue))]
  syntax Foo@TEST ::= "t" [Location(#token("7",Int@INT-SYNTAX),#token("18",Int@INT-SYNTAX),#token("7",Int@INT-SYNTAX),#token("36",Int@INT-SYNTAX)) Source(#token("src/test/resources/convertor-tests/syntaxWithPriorities.k",KString@KSTRING)) andanohter(#token("",AttributeValue)) klabel(#token("t",KString@KSTRING))]
endmodule
