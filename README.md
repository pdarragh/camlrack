# Camlrack: S-Expression Matching in OCaml

Camlrack brings [S-Expressions](https://en.wikipedia.org/wiki/S-expression) and
easy pattern matching for S-Expressions to OCaml like never before!


## Full Manual

There is a full manual for Camlrack at [`docs/Manual.md`](docs/Manual.md).


## Quick Overview

S-Expressions can be instantiated directly:

```ocaml
open Camlrack

(* Represent '(+ 1 2) *)
let se1 = SExp [ Symbol "+"; Integer 1; Integer 2 ]

(* Represent '(add1 x) *)
let se2 = SExp [ Symbol "add1"; Symbol "x" ]
```

They can also be parsed from strings:

```ocaml
open Camlrack

let se3 = sexp_of_string_exn "(let ([x 2] [y 3]) (+ x y))"
let se4 = SExp [ Symbol "let"
               ; SExp [ SExp [ Symbol "x"; Integer 2 ]
                      ; SExp [ Symbol "y"; Integer 3 ] ]
               ; SExp [ Symbol "+"; Symbol "x"; Symbol "y" ] ]
assert (s3 = se4)
```

S-Expressions can also be compared against S-Expression patterns. S-Expression
pattern matching is based entirely on the [PLAIT
language](https://docs.racket-lang.org/plait/index.html) implemented by Matthew
Flatt.

```ocaml
open Camlrack

let spat1 = SPat [ SYMBOL    ; INTEGER   ; PSymbol "literal" ]
let sexp1 = SExp [ Symbol "x"; Integer 42; Symbol  "literal" ]
assert (sexp_match spat1 sexp1)

let spat2 = sexp_pattern_of_string_exn "{lambda {SYMBOL ...} ANY ...}"
let sexp2 = sexp_of_string_exn "(lambda (x y) (+ x y))"
assert (sexp_match spat2 sexp2)
```


## PPX Extensions

Camlrack also provides two PPX extensions: `%sexp` and `%spat`.


### `%sexp`

The first can be used to either rewrite strings as S-Expressions (sometimes at
compile-time), or else to provide a more fully featured S-Expression matching
system.

```ocaml
let sexp1 = [%sexp "(foo bar)"]
let sexp2 = SExp [ Symbol "foo"; Symbol "bar" ]
assert (sexp1 = sexp2)

let do_a_match = function%sexp
  | (foo, bar) -> ...
  | (lambda, (x, y)) -> ...
  | "{lambda {x y}}" -> ... (* redundant; identical to previous case *)
```

Note that the non-string forms are somewhat limited. PPX extensions are
restricted to only supporting syntactically valid OCaml, so the tuple-like form
must be built of real tuples (meaning commas are required). Additionally,
symbols containing special characters such as those used by operators cannot be
handled in the tuple-like form.

For most cases, it is probably advisable to use the string form.


### `%spat`

The second extension allows for easy pattern-matching against S-Expressions.

```ocaml
let matching_function sexp = match%spat sexp with
  | "INTEGER" -> "it's an integer"
  | "(lhs rhs)" -> "found two symbols: lhs and rhs"
  | "(let ([SYMBOL ANY] ...) ANY ...)" -> "whoa, pretty fancy!"
```

This extension only works on `match` expressions, and the cases must be written
as string literals.
