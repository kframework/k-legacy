// Copyright (c) 2014 K Team. All Rights Reserved.

module TIME-HOOKS
  imports #TIME-INTERFACE

  syntax Int ::= "time" "(" ")" [function, impure, klabel(time), hook(#TIME:time)]

endmodule
