// Copyright (c) 2015-2016 K Team. All Rights Reserved.

// To be used when parsing/pretty-printing ground configurations
module KSEQ
  imports KAST
  imports K-TOP-SORT
  syntax KBott ::= ".K"      [klabel(#EmptyK), khook(org.kframework.kore.EmptyK)]
                 | "."       [klabel(#EmptyK), khook(org.kframework.kore.EmptyK)]
                 | ".::K"    [klabel(#EmptyK), khook(org.kframework.kore.EmptyK)]
                 | K "~>" K  [klabel(#KSequence), left, assoc, unit(#EmptyK), khook(org.kframework.kore.KSequence)]
  syntax left #KSequence
  syntax KBott     ::= "(" K ")"    [bracket]
endmodule

// To be used when parsing/pretty-printing symbolic configurations
module KSEQ-SYMBOLIC
  imports KSEQ
  syntax #KVariable ::= r"(?<![A-Za-z0-9_\\$!\\?])(\\!|\\?)?([A-Z][A-Za-z0-9'_]*|_)"   [token, autoReject, khook(org.kframework.kore.KVariable)]
  syntax KConfigVar ::= r"(?<![A-Za-z0-9_\\$!\\?])(\\$)([A-Z][A-Za-z0-9'_]*)"          [token, autoReject]
  syntax KBott      ::= #KVariable [allowChainSubsort]
  syntax KBott      ::= KConfigVar [allowChainSubsort]
  syntax KLabel     ::= #KVariable [allowChainSubsort]
endmodule

module KCELLS
  imports KAST

  syntax Cell
  syntax Bag ::= Bag Bag  [left, assoc, klabel(#cells), unit(#cells)]
               | ".Bag"   [klabel(#cells)]
               | ".::Bag" [klabel(#cells)]
               | Cell     [allowChainSubsort]
  syntax Bag ::= "(" Bag ")" [bracket]
  syntax K ::= Bag
  syntax Bag ::= KBott
endmodule

module RULE-CELLS
  imports KCELLS
  // if this module is imported, the parser automatically
  // generates, for all productions that have the attribute 'cell' or 'maincell',
  // a production like below:
  //syntax Cell ::= "<top>" #OptionalDots K #OptionalDots "</top>" [klabel(<top>)]

  syntax #OptionalDots ::= "..." [klabel(#dots)]
                         | ""    [klabel(#noDots)]
endmodule

module RULE-PARSER
  imports RULE-LISTS
  imports RULE-CELLS
  // imported in modules which generate rule parsers
  // TODO: (radumereuta) don't use modules as markers to generate parsers
endmodule

module CONFIG-CELLS
  imports KCELLS
  imports RULE-LISTS
  syntax #CellName ::= r"[a-zA-Z0-9\\-]+"  [token]

  syntax Cell ::= "<" #CellName #CellProperties ">" K "</" #CellName ">" [klabel(#configCell)]
  syntax Cell ::= "<" #CellName "/>" [klabel(#externalCell)]
  syntax Cell ::= "<br" "/>" [klabel(#breakCell)]

  syntax #CellProperties ::= #CellProperty #CellProperties [klabel(#cellPropertyList)]
                           | ""                            [klabel(#cellPropertyListTerminator)]
  syntax #CellProperty ::= #CellName "=" KString           [klabel(#cellProperty)]

endmodule

module REQUIRES-ENSURES
  imports BASIC-K

  syntax RuleContent ::= K                                 [klabel("#ruleNoConditions"), allowChainSubsort, latex({#1}{}{})]
                       | K "requires" K                    [klabel("#ruleRequires"), latex({#1}{#2}{})]
                       | K "when" K                        [klabel("#ruleRequires"), latex({#1}{#2}{})]
                       | K "ensures"  K                    [klabel("#ruleEnsures"), latex({#1}{}{#3})]
                       | K "requires" K "ensures" K        [klabel("#ruleRequiresEnsures"), latex({#1}{#2}{#3})]
                       | K "when" K "ensures" K            [klabel("#ruleRequiresEnsures"), latex({#1}{#2}{#3})]
endmodule

module K-TOP-SORT
  // if this module is imported, the parser automatically
  // generates, for all sorts, productions of the form:
  // K     ::= Sort
  // this is part of the mechanism that allows concrete user syntax in K
endmodule

module K-BOTTOM-SORT
  // if this module is imported, the parser automatically
  // generates, for all sorts, productions of the form:
  // Sort  ::= KBott
  // this is part of the mechanism that allows concrete user syntax in K
endmodule

module K-SORT-LATTICE
  imports K-TOP-SORT
  imports K-BOTTOM-SORT
endmodule

module AUTO-CASTS
  // if this module is imported, the parser automatically
  // generates, for all sorts, productions of the form:
  // Sort  ::= Sort "::Sort"
  // Sort  ::= Sort ":Sort"
  // KBott ::= Sort "<:Sort"
  // Sort  ::= K    ":>Sort"
  // this is part of the mechanism that allows concrete user syntax in K
endmodule

module AUTO-FOLLOW
  // if this module is imported, the parser automatically
  // generates a follow restriction for every terminal which is a prefix
  // of another terminal. This is useful to prevent ambiguities such as:
  // syntax K ::= "a"
  // syntax K ::= "b"
  // syntax K ::= "ab"
  // syntax K ::= K K
  // #parse("ab", "K")
  // In the above example, the terminal "a" is not allowed to be followed by a "b"
  // because it would turn the terminal into the terminal "ab".
endmodule

module PROGRAM-LISTS
  imports SORT-K
  // if this module is imported, the parser automatically
  // replaces the default productions for lists:
  // Es ::= E "," Es [userList("*"), klabel('_,_)]
  //      | ".Es"    [userList("*"), klabel('.Es)]
  // into a series of productions more suitable for programs:
  // Es#Terminator ::= ""      [klabel('.Es)]
  // Ne#Es ::= E "," Ne#Es     [klabel('_,_)]
  //         | E Es#Terminator [klabel('_,_)]
  // Es ::= Ne#Es
  //      | Es#Terminator      // if the list is *
endmodule

module RULE-LISTS
  // if this module is imported, the parser automatically
  // adds the subsort production to the parsing module only:
  // Es ::= E        [userList("*")]

endmodule

module DEFAULT-CONFIGURATION
  imports BASIC-K

  configuration <k> $PGM:K </k>
endmodule

// To be used to parse semantic rules
module K
  imports KSEQ-SYMBOLIC
  imports REQUIRES-ENSURES
  imports K-SORT-LATTICE
  imports AUTO-CASTS
  imports AUTO-FOLLOW
  syntax KBott     ::= K "=>" K     [klabel(#KRewrite), khook(org.kframework.kore.KRewrite), non-assoc]
  syntax non-assoc #KRewrite

endmodule

// To be used to parse terms in full K
module K-TERM
  imports KSEQ-SYMBOLIC
  imports K-SORT-LATTICE
  imports AUTO-CASTS
  imports AUTO-FOLLOW
endmodule
