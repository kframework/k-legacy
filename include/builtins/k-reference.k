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
K has predefined the lexer rules for the most common syntactic categories
 that are directly accessible through subsorting:

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

/*@ \section{Tags} \label{sec:TAGS} 
   Throughout \K, tags are used to specify options on both rewrite rules and
   syntax productions. Some of these options are fairly self-descriptive, while
   others can relate to more obscure aspects of \K. This section is meant to 
   serve as a list of available tags and to provide detailed information on
   them.  
*/

/*@ \subsection{Syntax Tags} \label{subsec:SYNTAX-TAGS}
   These tags are all used on syntax productions. 
*/

/*@ {[}\kattribute{bracket}{]} \\
   This production has no syntactic meaning outside of demarcating evaluation 
   boundaries. In other words, this production accomplishes what in most 
   programming languages is performed with the parentheses.
 */


/*@ {[}\kattribute{left}{]} \\
   This production obeys left-associativity.
 */

/*@ {[}\kattribute{right}{]} \\
   This production obeys right-associativity.
 */

/*@ {[}\kattribute{non-assoc}{]} \\
   This production is non-associative.
 */

/*@ {[}\kattribute{avoid}{]} , {[}\kattribute{prefer}{]} \\
   These tags are parser directives. They allow the user to indicate priority
   between two otherwise ambiguous productions: productions tagged 
   {[}\kattribute{avoid}{]} are given lower priority, and productions tagged
   {[}\kattribute{prefer}{]} are given higher priority. It may be possible to
   disambiguate such productions by restructuring the semantics, but these tags
   provide a convient way to avoid such hassle.
 */

/*@ {[}\kattribute{strict}{]} , {[}\kattribute{strict(Int)}{]} ,
    {[}\kattribute{seqstrict}{]} \\
   If no argument is given, all symbols in this rule will be
   evaluated to terminal symbols before further evaluation. If an integer
   argument is given, then only the symbol in that index (using 1-indexing) will
   be evaluated to a non-terminal. {[}\kattribute{seqstrict}{]} is shorthand
   for evaluating all symbols from left to right to non-terminals. (TODO: I'm
   pretty sure you can get right-to-left strict evaluation with seqstrict, but
   I'm not sure how.)
 */

/*@ {[}\kattribute{function}{]} \\
   Treat this production as a builtin function.
   TODO: I don't understand functions very well. I want somebody else to write
   this one.
 */

/*@ {[}\kattribute{klabel}{]} \\
   TODO: I don't know what klabels are or why they're important.
 */

/*@ {[}\kattribute{notInRules}{]} , {[}\kattribute{notInPrograms}{]} , 
   {[}\kattribute{notInGround}{]}, {[}\kattribute{onlyLabel}{]} \\

   There are 3 parser generated at kompile time:
   \begin{enumerate}
   \item The program parser. This takes the exact syntax of the syntax module and
    generates a parser from that. 
   \item The rule parser. This takes all the syntax declarations from all included
    modules and mixes it with \K (cells, meta-variables, lists, bags...etc),
    except for Ids.
   \item The ground parser. Just like the above, except for meta-variables,
    which are replaced with Ids.
   \end{enumerate}

   These three tags are used to declare that a production should not be included
   in a particular parser, according to the name used in the tag. This is
   useful primarily for avoiding ambiguities, especially with meta-variables.

   {[}\kattribute{onlyLabel}{]} is considered depricated, and is equivalent to
   {[}\kattribute{notInRules}{]} and {[}\kattribute{notInGround}{]} at the same
   time.

 */

/*@ {[}\kattribute{latex(...)}{]} \\
   Indicate how this production should be displayed in \LaTeX mode, i.e when
   kompiling with the option --backend=pdf or --backend=latex. This is used
   primarily to specify display options on non-standard symbols like $\leq$.
   The \kattribute{latex} option uses \{\#X\} to refer to symbol X using
   1-indexing, and arbitrary \LaTeX formatting around them. For instance , \\

   syntax Bool ::= Exp "<=" Exp {[}latex(\{\#1\}\backslash leq\{\#2\}){]} \\

   is rendered \\

\begin{syntaxBlock}
{\nonTerminal{\sort{Bool}}}\syntax{{{\nonTerminal{\sort{Exp}}}}\leq{{\nonTerminal{\sort{Exp}}}}}{}
\end{syntaxBlock}

 */


/*@ \subsection{Rule Tags} \label{subsec:RULE-TAGS}
   These tags are all used on rewrite rules. Note that ruels can be tagged with
   arbitrary attributes; if an attribute is not already be in use by \K, it will
   be treated simply as a label, and can be used to include the rule in some
   --kompile options, such as --superheat and --supercool.
 
*/

/*@ {[}\kattribute{binder}{]} \\
   This rule binds a variable to a value. This is particularly important when
   using the substitution module, as it is necessary to avoid variable capture
   problems.
 */

/*@ {[}\kattribute{anywhere}{]} \\
   This rule can be applied anywhere it occurs, not simply when it's at the head
   of the execution cell.
 */

/*@ {[}\kattribute{macro}{]} \\
   Before execution of the program, apply this rules wherever it applies. These
   rules are treated as {[}\kattribute{anywhere}{]} rules. In many instances,
   the {[}\kattribute{macro}{]} tag is used to dissolve syntactic sugar into
   more easily processable chunks.
 */




/*@ {[}\kattribute{structural}{]} \\
\K rules are of two types: structural and computational.  Intuitively,
structural rules rearrange the configuration so that computational rules can
apply.  Structural rules therefore do not count as computational steps.  A \K
semantics can be thought of as a generator of transition systems, one for each
program.  It is only the computational rules that create steps, or transitions,
in the corresponding transition system, the structural rules being unobservable
at this level. By default, rules are all assumed computational, except for
the implicit heating/cooling rules that define evaluation strategies of
language constructs, which are assumed structural.  If you want to explicitly
make a rule structural, then you should include a {[}\kattribute{structural}{]}
tag. These attributes may be taken into account by different \K tools, so it is
highly recommended to spend a moment or two after each rule and think whether 
you want it to be structural or computational.
 */

/*@ {[}\kattribute{superheat}{]}, {[}\kattribute{supercool}{]} \\
These tags are used to tell `kompile' what kind of language model we are
interested for analysis purposes.  When we experiment with non-determinism due
to evaluation strategies of language constructs, we should keep in mind that
`kompile' allows us to configure two important parameters, called `--superheat`,
and respectively, `--supercool`:

1. `superheat`: what language constructs we want the generated language model
to allow for exhaustive non-deterministic analysis; and
2. `supercool`: what rules we want to trigger the search for a *next behavior*.

If you want to explore the entire behavior space due to non-deterministic
evaluation strategies, then you should include all the language constructs 
in the `--superheat' option and all the rules in the `--supercool' option.
In large languages, however, this explodes the state space, causing 
too many behaviors for `krun' to consider, and it will likely hang
or crash. Driven by practical needs, the \K tool therefore allows you to finely 
tune the generated language models using these two options above. 
Metaphorically, the `super' prefix is meant to fully incorporate in
the generated language model the theoretical meaning of the corresponding
operation: `superheat' indicates strict language constructs whose heating
rules have to be considered as backtracking markers for search, and
`supercool' indicates rules which enforce the application of the corresponding
cooling rules of those constructs.

It can be useful to tag some rules with arbitrary labels that are not used 
internally by \K : These tags can be passed as arguments to the kompile option 
--superheat or --supercool, and they will be treated as though they were 
{[}\kattribute{superheat}{]} or {[}\kattribute{supercool}{]} tags, respectively.

 */

/*@ {[}\kattribute{transition}{]} \\
   Similarly to the {[}\kattribute{superheat}{]} and 
   {[}\kattribute{supercool}{]} rules, this is a tag that lets kompile know how
   to interpret semantics, and is essential for being able to identify
   non-deterministic behaviors with krun. Rules with a 
   {[}\kattribute{transition}{]} tag are considered rules that could potentially
    transition the program into more than one behavior. Generally, rules that
   manage variable lookup or printing, which may exhibit non-deterministic 
   behaviors especially in multi-threaded environments, should be tagged 
   {[}\kattribute{transition}{]} .

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
