require "k-prelude.k"

module K-REFERENCE

/*@ \section{Overview}
   \label{sec:Overview}
TLDR: press ctrl+f and search for keywords. Try clicking links to things that
 look relevant. If there's information missing or misplaced, please send an 
email to (TODO: get an email address to put here) detailing the nature of the
 omission or misplacement.\\

This reference manual is designed to provide answers to questions that tend to 
arise over the course of using \K. In it, several sections are devoted to 
features of \K and \K usage, while the remaining sections are devoted to 
providing documentation on \K's builtin classes and structures. The document is 
entirely searchable; ctrl+f is probably the most straightforward method of 
trying to find relevant information. Links to the builtin modules are provided 
at the beginning of the document. This manual is a work in progress; if there's a 
question whose solution you were unable to find in this manual, please send an 
email to (TODO: get email address) detailing the nature of the question and a 
suggestion as to where it might best be placed.
*/



/*@ \section{Builtin Literals}
K has predefined the lexer rules for the most common syntactic categories that are directly accessible through subsorting:
\begin{verbatim}
syntax #Bool   ::= Token{"true"} | Token{"false"}
syntax #Id     ::= Token{[a-zA-Z][a-zA-Z0-9]*}
syntax #Int    ::= Token{[\-\+]? [0-9]+}
syntax #String ::= Lexer{"\"" (~[\"\\] | "\\\"" | "\\")* "\""}
                   // where '\' cannot be followed by double quote
syntax #Float  ::= Lexer{[0-9]+ "." [0-9]+ ([eE] #Int)}
syntax #Float  -/- [a-zA-Z0-9\.]
\end{verbatim}

For some languages these may not be enough so you might want to declare your
own.  There are two main things that you have to consider: the regular
expression that will do the matching, and the follow restriction. The latter
tells the regular expression when not to stop matching.

A regular expression for the matcher is similar to the one in SDF (TODO: What is SDF?). Basically,
you may write strings, sets of tokens (using [...]), and add optional arguments
with '?'. You can negate a set of tokens by prefixing it with '\texttildelow'.
Inside a character set you may write individual characters or entire ranges:
a-z A-Z 0-9. Anything other than these needs to be escaped using backslash.

The follow restriction is an important feature of the SDF parser, as the
lexical rules are not greedy by default. The last line in the example above,
says that \#Float cannot be followed by one of the characters in the set. The
list can be extended to check multiple characters ahead by concatenating sets
with '.' (\verb^syntax X -/- [a].[b].[c]^).

Sometimes you may also want to specify follow restrictions for keywords to
avoid some ambiguities:

\verb^syntax "keyword" -/- [a-zA-Z0-9]^

Another useful construction is a list separated by a character. For example the
name of a module in K is matched by a construction like the following:

\begin{verbatim}
syntax MODNAME ::= Lexer{"#"?{[a-z0-9A-Z\_]+ "-"}+}
syntax MODNAME -/- [a-z0-9A-Z\-\_]
\end{verbatim}

The construction \{<char set> "<separator>"\}+ will match 'A', 'A-A', 'A-A-A',
but not 'A-'.

\verb^Lexer{...}^ and \verb^-/-^ are the main way of specifying a lexical rule,
but if your expression is simple enough, you may write directly
\verb^Token{...}^. This will try to infer automatically the follow restriction
(if needed). Looking back at the rule for \#Id, it will be equivalent to:

\begin{verbatim}
syntax #Id ::= Lexer{[a-zA-Z][a-zA-Z0-9]*}
syntax #Id -/- [a-zA-Z0-9]
\end{verbatim}

Note that this works for simple expressions that have a repeated character set
at the end. If you want to specify something more complex, you will have to use
\verb^Lexer{...}^ and specify the follow restriction.

*/



imports DEFAULT-CONFIGURATION
imports SET
imports MAP
imports LIST
imports K-EQUAL-JOOKS
imports BOOL-HOOKS
imports INT-HOOKS
imports STRING-HOOKS
imports FLOAT-HOOKS
imports ID-HOOKS
imports K-RANDOM





endmodule
