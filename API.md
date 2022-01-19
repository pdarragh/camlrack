# Camlrack API

This document provides information about all of the types and functions exported
by Camlrack.


## Types

### `sexp`: S-Expressions <a id="sexp">

S-Expressions are either built of fundamental symbols or lists of S-Expressions:

```ocaml
type sexp =
  | Integer of int
  | Float of float
  | String of string
  | Symbol of string
  | SExp of sexp list
```

  * `sexp` : S-Expressions are composed of...
      * `Integer` of `int` : integers.
      * `Float` of `float` : floating-point numbers.
      * `String` of `string` : strings.
      * `Symbol` of `string` : symbols.
      * `SExp` of `sexp` `list` : sub-lists of S-Expressions.

### `sexp_pattern`: Patterns for Matching S-Expressions

Sometimes one does not wish to match S-Expressions literally, but would instead
prefer a regex-like experience for matching against *shapes* of S-Expressions.
The `sexp_pattern` type is used for specifying such shapes:

```ocaml
type sexp_pattern =
  | SYMBOL
  | INTEGER
  | FLOAT
  | STRING
  | ANY
  | PInteger of int
  | PFloat of float
  | PString of string
  | PSymbol of string
  | SPat of sexp_pattern list
```

  * `sexp_pattern` : S-Expression matching patterns are composed of...
      * `SYMBOL` : a `Symbol` wildcard.
      * `INTEGER` : an `Integer` wildcard.
      * `FLOAT` : a `Float` wildcard.
      * `STRING` : a `String` wildcard.
      * `ANY` : an `sexp` wildcard.
      * `PInteger` of `int` : an exact match for an `Integer`.
      * `PFloat` of `float` : an exact match for a `Float`.
      * `PString` of `string` : an exact match for a `String`.
      * `PSymbol` of `string` : an exact match for a `Symbol`.
      * `SPat` of `sexp_pattern` `list` : an exact match for a sub-list of
        S-Expressions.


## Functions

### Parsing Strings to S-Expressions

  * `sexp_of_string` : `string` &rarr; [`sexp`](#sexp) `option` <a
    id="sexp_of_string">

    Attempts to convert a string to an [`sexp`](#sexp)

  * `sexp_of_string_exn` : `string` &rarr; [`sexp`](#sexp) <a
    id="sexp_of_string_exn">

    Converts a string to an [`sexp`](#sexp), or else raises an exception.


### Converting Whole S-Expressions to Strings

  * `render_string_of_sexp` : [`sexp`](#sexp) &rarr; `string`

    Converts an [`sexp`](#sexp) to a string representing that S-Expression. For
    example:

    ```ocaml
    # render_string_of_sexp (SExp [Symbol "foo"; Integer 42]);;
    - : string = "(foo 42)"
    ```

  * `repr_string_of_sexp` : [`sexp`](#sexp) &rarr; `string`

    Converts an [`sexp`](#sexp) to a string that is explicitly annotated, useful
    for debugging but not much else. For example:

    ```ocaml
    # repr_string_of_sexp (SExp [Symbol "foo"; Integer 42]);;
    - : string = "SExp [Symbol \"foo\"; Integer 42]"
    ```


### Other S-Expression Conversion Functions

  * `sexp_to_list_opt` : [`sexp`](#sexp) &rarr; ([`sexp`](#sexp) `list`)
    `option`

    Given an `SExp` variant of [`sexp`](#sexp), returns `Some` internal list of
    S-Expressions. Otherwise, returns `None`.

    ```ocaml
    # sexp_to_list_opt (SExp [Integer 1; Integer 2]);;
    - : sexp list option = Some [Integer 1; Integer 2]
    # sexp_to_list_opt (Integer 3);;
    - : sexp list option = None
    ```

  * `sexp_to_list` : [`sexp`](#sexp) &rarr; [`sexp`](#sexp) `list`

    Given an `SExp` variant of [`sexp`](#sexp), returns the internal list of
    S-Expressions. Otherwise, raises an exception.
