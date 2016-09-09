// Copyright (c) 2016 K Team. All Rights Reserved.
module ANOTHER [Location(#token("3",Int@INT-SYNTAX),#token("1",Int@INT-SYNTAX),#token("4",Int@INT-SYNTAX),#token("9",Int@INT-SYNTAX)) Source(#token("src/test/resources/convertor-tests/imports.k",KString@KSTRING))]
endmodule


module TEST [Location(#token("6",Int@INT-SYNTAX),#token("1",Int@INT-SYNTAX),#token("8",Int@INT-SYNTAX),#token("9",Int@INT-SYNTAX)) Source(#token("src/test/resources/convertor-tests/imports.k",KString@KSTRING))]
  imports ANOTHER
endmodule
