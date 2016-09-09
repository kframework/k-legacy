// Copyright (c) 2016 K Team. All Rights Reserved.
module TEST [Location(#token("3",Int@INT-SYNTAX),#token("1",Int@INT-SYNTAX),#token("6",Int@INT-SYNTAX),#token("9",Int@INT-SYNTAX)) Source(#token("src/test/resources/convertor-tests/syntaxWithRhs.k",KString@KSTRING))]
  syntax Foo@TEST ::= "1" [Location(#token("4",Int@INT-SYNTAX),#token("18",Int@INT-SYNTAX),#token("4",Int@INT-SYNTAX),#token("20",Int@INT-SYNTAX)) Source(#token("src/test/resources/convertor-tests/syntaxWithRhs.k",KString@KSTRING)) klabel(#token("1",KString@KSTRING))]
  syntax Bar@TEST ::= "2" [Location(#token("5",Int@INT-SYNTAX),#token("18",Int@INT-SYNTAX),#token("5",Int@INT-SYNTAX),#token("32",Int@INT-SYNTAX)) Source(#token("src/test/resources/convertor-tests/syntaxWithRhs.k",KString@KSTRING)) klabel(#token("2",KString@KSTRING)) some(#token("foo",AttributeValue))]
endmodule
