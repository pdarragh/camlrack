# Camlrack Manual

This document provides the complete manual for using
[Camlrack](https://github.com/pdarragh/camlrack), an S-Expression
pattern-matching library for OCaml.

**Note on platforms.** This guide is written assuming the reader is using a
Unix-like environment. If you are on Windows and not using the Windows Subsystem
for Linux (WSL), I cannot help you because I do not have a Windows machine to do
any testing. I use macOS, but I believe the instructions should be sufficiently
platform-agnostic to work for Linux or WSL.


## Table of Contents

  * [Feedback](#feedback)
  * [Installation](#installation)
      * [From OPAM](#from-opam)
      * [From Source](#from-source)
      * [Including Camlrack in Your Code](#including-camlrack-in-your-code)
  * [Camlrack API](#camlrack-api)
  * [Using Camlrack](#using-camlrack)
  * [Matching S-Expressions with
    Patterns](#matching-s-expressions-with-patterns)
  * [Statically Producing S-Expressions from
    Strings](#statically-producing-s-expressions-from-strings)
  * [Pattern-Matching Against
    S-Expressions](#pattern-matching-against-s-expressions)
  * [Pattern-Matching Against S-Expressions: Part
    2](#pattern-matching-against-s-expressions-part-2)


## Feedback

If you have comments, questions, find any bugs, or otherwise want to send
feedback, please file an issue on the GitHub repository: [pdarragh/camlrack
Issues](https://github.com/pdarragh/camlrack/issues).


## Installation

Camlrack requires OCaml 4.12 or greater. You can install OCaml by following
[these instructions](https://ocaml.org/learn/tutorials/up_and_running.html).


### From OPAM

You can install Camlrack and its PPX extensions from OPAM by doing:

```text
$ opam install camlrack ppx_camlrack
```


### From Source

Camlrack's PPX extensions rely on the `ppxlib` library, so you'll need that
first. To install it, do:

```text
$ opam install ppxlib
```

clone [the Camlrack repository from
GitHub](https://github.com/pdarragh/camlrack) to your local computer and install
it using [Dune](https://dune.build):

```text
$ cd ~/some/coding/directory/
$ git clone https://github.com/pdarragh/camlrack.git
$ cd camlrack
$ dune build @install
$ dune install
```

To update Camlrack when installed from source, you can pull the latest changes
from the git repository and re-install the library:

```text
$ cd ~/some/coding/directory/camlrack/
$ git pull
$ dune build @install
$ dune install
```


### Including Camlrack in Your Code

Once installation is complete, both `camlrack` and `ppx_camlrack` will be
available through Dune, and you can now add Camlrack as a dependency for any
project built with Dune by using the `libraries` directive:

```dune
(library
 (public_name my_cool_library)
 ...
 (libraries camlrack)
 ...)
```

To use the PPX extensions, you can additionally use the `preprocess` directive:

```dune
(library
 (public_name my_cool_library)
 ...
 (libraries camlrack)
 (preprocess (pps ppx_camlrack))
 ...)
```


## Camlrack API

The full API for Camlrack is provided [in another file](Camlrack_API.md).


## Using Camlrack

We assume familiarity with
[S-Expressions](https://en.wikipedia.org/wiki/S-expression). S-Expressions are a
concise way to represent recursive list-like data structures using only
primitive data types (integers, floats, strings, or symbols) and brackets
(parentheses, square brackets, or curly braces).

A string representing an S-Expression can be parsed to an
[`sexp`](Camlrack_API.md#sexp) by using
[`sexp_of_string`](Camlrack_API.md#sexp_of_string):

```ocaml
open Camlrack

(* Get a string from somewhere. *)
let sexp_string = "(some [string \"containing\" 5] atoms)"

(* Then, convert the string to an S-Expression: *)
let sexp = sexp_of_string_opt sexp_string

(* We could have built the same S-Expression manually like so: *)
let manual_sexp =
  Some (SExp [ Symbol "some"
             ; SExp [ Symbol "string"
                    ; String "containing"
                    ; Integer 5 ]
             ; Symbol "atoms" ])
```


## Matching S-Expressions with Patterns

The true utility of Camlrack lies in the ability to compare concrete
S-Expressions ([`sexp`](Camlrack_API.md#sexp)s) to abstract S-Expression
patterns ([`sexp_pattern`](Camlrack_API.md#sexp_pattern)s). For example, if you
wanted to check whether an S-Expression given by the user corresponds to the
Scheme-like syntax of a function definition:

```ocaml
open Camlrack

let is_function (se : sexp) : bool =
    match_sexp
      (SPat [ PSymbol "lambda"
            ; SPat [ SYMBOL
                   ; PSymbol "..." ]
            ; ANY
            ; PSymbol "..." ])
      se
```

Alternatively, we could use a convenience function to write that pattern in a
string and convert it to an S-Expression pattern dynamically:

```ocaml
open Camlrack

let function_pattern = "{lambda {SYMBOL ...} ANY ...}"
let function_sexp_pattern = sexp_pattern_of_string_exn fucntion_pattern

let is_function (se : sexp) : bool =
    match_sexp function_sexp_pattern se
```

(Although Camlrack doesn't care what style of brackets you use when writing such
patterns, it is customary to write patterns' lists with curly braces.)


## Statically Producing S-Expressions from Strings

The `ppx_camlrack` package provides some macro forms that allow for more
syntactically friendly manipulation of S-Expressions. One of these manipulations
uses the `%sexp` extension, which allows you to convert string literals into
S-Expressions at compile-time (rather than at run-time).

Assuming you are using `ppx_camlrack` as a rewriter (see [Including Camlrack in
Your Code](#including-camlrack-in-your-code)), you can use the `%sexp` extension
to convert strings at compile-time like so:

```ocaml
let sexp_string : string = "(one (two three))"
let manual_sexp : sexp = SExp [ Symbol "one"
                              ; SExp [ Symbol "two"
                                     ; Symbol "three" ]]
let compiled_sexp : sexp = [%sexp "(one (two three))"]
```


## Pattern-Matching Against S-Expressions

Sometimes, it may be useful to pattern-match against S-Expressions. The `%sexp`
extension also allows for writing S-Expressions as match cases. Consider the
following function:

```ocaml
let do_a_match (se : sexp) =
    match se with
    | Symbol "one" -> "found a 'one'"
    | SExp [Symbol "one"; Symbol "two"] -> "found a 'one' and a 'two'"
    | _ -> "found something else"
```

Clearly, matching against S-Expressions can be very verbose. They take a lot of
room to write out! The `%sexp` extension can be used on `match` expressions to
simplify this:

```ocaml
let do_a_match (se : sexp) =
    match%sexp se with
    | "one" -> "found a 'one'"
    | "(one two)" -> "found a 'one' and a 'two'"
    | _ -> "found something else"
```

The `match%sexp` form also provides the ability to match S-Expressions with a
tuple-like syntax, though this does not allow for matching against literal
symbols:

```ocaml
let a_different_match (se : sexp) =
    match%sexp se with
    | (lhs, rhs) -> "found a pair of " ^ lhs ^ " and " ^ rhs
    | (lhs, (ilhs, irhs)) -> "found a nested tuple: " ^ lhs ^ " " ^ ilhs ^ " " ^ irhs
    | _ -> "found something else"
```


## Pattern-Matching Against S-Expressions: Part 2

Often, it is more useful to pattern-match S-Expressions using the more abstract
comparison provided by [`sexp_pattern`](Camlrack_API.md#sexp_pattern)s, rather
than the literal form provided by
[`match%sexp`](#pattern-matching-against-s-expressions). The `ppx_camlrack`
library also provides an extension to help with this, called `match%spat`.

The `match%spat` expression only accepts string literals as cases, but it
transforms those strings into [`sexp_pattern`](Camlrack_API.md#sexp_pattern)s at
compile-time. This is often useful for writing parsers where your input language
is S-Expressions. For example, consider writing a parser for a small language
that contains integers, variables, binary addition expressions, and binary
multiplication expressions. We can implement this parser:

```ocaml
open Camlrack
open Camlrack.ListConvenienceFunctions

type exp =
  | Eint of int
  | Eid of string
  | Eplus of exp * exp
  | Emult of exp * exp

let rec parse (s : sexp) : exp =
  match%spat s with
  | "INTEGER" -> Eint (sexp_to_int s)
  | "SYMBOL" -> Eid (sexp_to_symbol s)
  | "{+ ANY ANY}" ->
    Eplus (parse (second (sexp_to_list s)),
           parse (third (sexp_to_list s)))
  | "{* ANY ANY}" ->
    Emult (parse (second (sexp_to_list s)),
           parse (third (sexp_to_list s)))
  | _ -> failwith "invalid input"
```

This is simpler to write and easier to read than the same parser without
`match%spat`:

```ocaml
open Camlrack
open Camlrack.ListConvenienceFunctions

type exp =
  | Eint of int
  | Eid of string
  | Eplus of exp * exp
  | Emult of exp * exp

let rec parse (s : sexp) : exp =
  if match_sexp INTEGER s
  then Eint (sexp_to_int s)
  else if match_sexp SYMBOL s
  then Eid (sexp_to_symbol s)
  else if match_sexp (SPat [Symbol "+"; ANY; ANY])
  then Eplus (parse (second (sexp_to_list s)), parse (third (sexp_to_list s)))
  else if match_sexp (SPat [Symbol "*"; ANY; ANY])
  then Emult (parse (second (sexp_to_list s)), parse (third (sexp_to_list s)))
  else failwith "invalid input"
```

Actually, `match%spat` converts the generated
[`sexp_pattern`](Camlrack_API.md#sexp_pattern) to a literal
[`sexp`](Camlrack_API.md#sexp) with wildcards in the place of literals. This
allows for the exhaustiveness checks of a typical `match` expression with the
convenience of writing [`sexp_pattern`](Camlrack_API.md#sexp_pattern)s as string
literals. The above `match%spat` parser is expanded at compile-time to something
like the following (where the bodies of the cases have been removed for
brevity):

```ocaml
let rec parse (s : sexp) : exp =
  match s with
  | Integer _ -> ...
  | Symbol _ -> ...
  | SExp [Symbol "+"; _; _] -> ...
  | SExp [Symbol "*"; _; _] -> ...
  | _ -> ...
```
