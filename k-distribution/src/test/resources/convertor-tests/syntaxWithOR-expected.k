// Copyright (c) 2016 K Team. All Rights Reserved.
module TEST [Location(#token("3",Int@INT-SYNTAX),#token("1",Int@INT-SYNTAX),#token("5",Int@INT-SYNTAX),#token("9",Int@INT-SYNTAX)) Source(#token("src/test/resources/convertor-tests/syntaxWithOR.k",KString@KSTRING))]
  syntax Foo@TEST ::= "y" [Location(#token("4",Int@INT-SYNTAX),#token("25",Int@INT-SYNTAX),#token("4",Int@INT-SYNTAX),#token("27",Int@INT-SYNTAX)) Source(#token("src/test/resources/convertor-tests/syntaxWithOR.k",KString@KSTRING)) klabel(#token("y",KString@KSTRING))]
  syntax Foo@TEST ::= "x" [Location(#token("4",Int@INT-SYNTAX),#token("19",Int@INT-SYNTAX),#token("4",Int@INT-SYNTAX),#token("21",Int@INT-SYNTAX)) Source(#token("src/test/resources/convertor-tests/syntaxWithOR.k",KString@KSTRING)) klabel(#token("x",KString@KSTRING))]
endmodule
