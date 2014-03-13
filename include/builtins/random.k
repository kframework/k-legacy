require "int.k"

module RANDOM-HOOKS
  imports #RANDOM-INTERFACE
  imports INT-HOOKS

  /*@ TODO: What is this? A random number generator with a seed?  */
  syntax Int ::= "randomRandom" "(" Int ")" [function, klabel(randomRandom), hook(#RANDOM:randomRandom)]

endmodule
