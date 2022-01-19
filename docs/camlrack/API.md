# Camlrack API

This document provides information about all of the types and functions exported
by Camlrack.


## Table of Contents

  * [Types](#types)
      * [S-Expressions](#s-expressions)
          * [`sexp`](#sexp)
      * [S-Expression Patterns](#s-expression-patterns)
          * [`sexp_pattern`](#sexp_pattern)
  * [Functions](#functions)
      * [Parsing Strings to S-Expressions](#parsing-strings-to-s-expressions)
          * [`sexp_of_string` : `string` &rarr; `sexp`
            `option`](#sexp_of_string)
          * [`sexp_of_string_exn : string -> sexp`](#sexp_of_string_exn)
      * [Converting Whole S-Expressions to
        Strings](#converting-whole-s-expressions-to-strings)
          * [`render_string_of_sexp`](#render_string_of_sexp)
          * [`repr_string_of_sexp`](#repr_string_of_sexp)
      * [Other S-Expression Conversion
        Functions](#other-s-expression-conversion-functions)
          * [`sexp_to_list_opt`](#sexp_to_list_opt)
          * [`sexp_to_list`](#sexp_to_list)
          * [`list_to_sexp`](#list_to_sexp)
          * [`sexp_to_int_opt`](#sexp_to_int_opt)
          * [`sexp_to_int`](#sexp_to_int)
          * [`int_to_sexp`](#int_to_sexp)
          * [`sexp_to_float_opt`](#sexp_to_float_opt)
          * [`sexp_to_float`](#sexp_to_float)
          * [`float_to_sexp`](#float_to_sexp)
          * [`sexp_to_string_opt`](#sexp_to_string_opt)
          * [`sexp_to_string`](#sexp_to_string)
          * [`string_to_sexp`](#string_to_sexp)
          * [`sexp_to_symbol_opt`](#sexp_to_symbol_opt)
          * [`sexp_to_symbol`](#sexp_to_symbol)
          * [`symbol_to_sexp`](#symbol_to_sexp)
      * [Parsing S-Expression
        Patterns](#parsing-s-expression-patterns)
          * [`sexp_pattern_of_string`](#sexp_pattern_of_string)
          * [`sexp_pattern_of_string_exn`](#sexp_pattern_of_string_exn)
          * [`sexp_pattern_of_sexp`](#sexp_pattern_of_sexp)
      * [Converting Whole S-Expression Patterns to
        Strings](#converting-whole-s-expression-patterns-to-strings)
          * [`render_string_of_sexp_pattern`](#render_string_of_sexp_pattern)
          * [`repr_string_of_sexp_pattern`](#repr_string_of_sexp_pattern)
      * [Matching S-Expressions Using S-Expression
        Patterns](#matching-s-expressions-using-s-expression-patterns)
          * [`sexp_match`](#sexp_match)


## Types


### S-Expressions

  * <a id="sexp" /> `sexp`:
    S-Expressions are composed of...
      * <a id="Integer" /> `Integer` of `int`:
        Integer literals.
      * <a id="Float" /> `Float` of `float`:
        Floating-point literals.
      * <a id="String" /> `String` of `string`:
        String literals.
      * <a id="Symbol" /> `Symbol` of `string`:
        Symbols. (Not quite the same as [`String`](#String)s.)
      * <a id="sexp-SExp" /> `SExp` of [`sexp`](#sexp) `list`:
        Sub-lists of [`sexp`](#sexp)s.


### S-Expression Patterns

Sometimes one does not wish to match S-Expressions literally, but would instead
prefer a regex-like experience for matching against *shapes* of S-Expressions.
The `sexp_pattern` type is used for specifying such shapes:

  * <a id="sexp_pattern" /> `sexp_pattern`:
    S-Expression matching patterns are composed of...
      * <a id="P_SYMBOL" /> `SYMBOL`:
        [`Symbol`](#Symbol) wildcards.
      * <a id="P_INTEGER" /> `INTEGER`:
        [`Integer`](#Integer) wildcards.
      * <a id="P_FLOAT" /> `FLOAT`:
        [`Float`](#Float) wildcards.
      * <a id="P_STRING" /> `STRING`:
        [`String`](#String) wildcards.
      * <a id="P_ANY" /> `ANY`:
        Generic [`sexp`](#sexp) wildcards.
      * <a id="PInteger" /> `PInteger` of `int`:
        Literal matches for [`Integer`](#Integer)s.
      * <a id="PFloat" /> `PFloat` of `float`:
        Literal matches for [`Float`](#Float)s.
      * <a id="PString" /> `PString` of `string`:
        Literal matches for [`String`](#String)s.
      * <a id="PSymbol" /> `PSymbol` of `string`:
        Literal matches for [`Symbol`](#Symbol)s.
      * <a id="SPat" /> `SPat` of [`sexp_pattern`](#sexp_pattern) `list`:
        Literal matches for sub-lists of [`sexp`](#sexp)s.


## Functions

### Parsing Strings to S-Expressions

  * <a id="sexp_of_string" /> `sexp_of_string` : `string` &rarr; [`sexp`](#sexp)
    `option`

    Attempts to convert a string to an [`sexp`](#sexp).

    ```ocaml
    # sexp_of_string "(one (2 3.0) \"two times two\")";;
    - : sexp option =
    Some (SExp [Symbol "one"; SExp [Integer 2; Float 3.]; String "two times two"])
    # sexp_of_string "((one)";;
    - : sexp option = None
    ```

  * <a id="sexp_of_string_exn" /> `sexp_of_string_exn` : `string` &rarr;
    [`sexp`](#sexp)

    Converts a string to an [`sexp`](#sexp), or else raises an exception.

    ```ocaml
    # sexp_of_string_exn "(one (2 3.0) \"two times two\")";;
    - : sexp =
    SExp [Symbol "one"; SExp [Integer 2; Float 3.]; String "two times two"]
    # sexp_of_string_exn "((one)";;
    Exception: Failure "unterminated S-expression".
    ```


### Converting Whole S-Expressions to Strings

  * <a id="render_string_of_sexp" /> `render_string_of_sexp` : [`sexp`](#sexp)
    &rarr; `string`

    Converts an [`sexp`](#sexp) to a string representing that S-Expression.

    ```ocaml
    # render_string_of_sexp (SExp [Symbol "foo"; Integer 42]);;
    - : string = "(foo 42)"
    ```

  * <a id ="repr_string_of_sexp" /> `repr_string_of_sexp` : [`sexp`](#sexp)
    &rarr; `string`

    Converts an [`sexp`](#sexp) to a string that is explicitly annotated, useful
    for debugging but not much else.

    ```ocaml
    # repr_string_of_sexp (SExp [Symbol "foo"; Integer 42]);;
    - : string = "SExp [Symbol \"foo\"; Integer 42]"
    ```


### Other S-Expression Conversion Functions

  * <a id ="sexp_to_list_opt" /> `sexp_to_list_opt` : [`sexp`](#sexp) &rarr;
    ([`sexp`](#sexp) `list`) `option`

    Given an [`SExp`](#sexp-SExp) variant of [`sexp`](#sexp), returns a `Some`
    containing the internal list of S-Expressions. Otherwise, returns `None`.

    ```ocaml
    # sexp_to_list_opt (SExp [Integer 1; Integer 2]);;
    - : sexp list option = Some [Integer 1; Integer 2]
    # sexp_to_list_opt (Integer 3);;
    - : sexp list option = None
    ```

  * <a id ="sexp_to_list" /> `sexp_to_list` : [`sexp`](#sexp) &rarr;
    [`sexp`](#sexp) `list`

    Given an [`SExp`](#sexp-SExp) variant of [`sexp`](#sexp), returns the
    internal list of S-Expressions. Otherwise, raises an exception.

    ```ocaml
    # sexp_to_list (SExp [Integer 1; Integer 2]);;
    - : sexp list option = [Integer 1; Integer 2]
    # sexp_to_list (Integer 3);;
    Exception: Failure "S-Expression is not a list: 3".
    ```

  * <a id="list_to_sexp" /> `list_to_sexp` : [`sexp`](#sexp) `list` &rarr;
    [`sexp`](#sexp)

    Given a list of [`sexp`](#sexp)s, produces a single [`SExp`](#sexp-SExp).

    ```ocaml
    # list_to_sexp [Integer 1; Integer 2];;
    - : sexp = SExp [Integer 1; Integer 2]
    ```

  * <a id="sexp_to_int_opt" /> `sexp_to_int_opt` : [`sexp`](#sexp) &rarr; `int`
    `option`

    Given an [`Integer`](#Integer), returns a `Some` containing the internal
    `int`. Otherwise, returns `None`.

    ```ocaml
    # sexp_to_int_opt (Integer 1);;
    - : int option = Some 1
    # sexp_to_int_opt (String "not an integer");;
    - : int option = None
    ```

  * <a id="sexp_to_int" /> `sexp_to_int` : [`sexp`](#sexp) &rarr; `int`

    Given an [`Integer`](#Integer), returns the internal `int`. Otherwise,
    raises an exception.

    ```ocaml
    # sexp_to_int (Integer 1);;
    - : int = 1
    # sexp_to_int (String "not an integer");;
    Exception: Failure "S-Expression is not an integer: \"not an integer\"".
    ```

  * <a id="int_to_sexp" /> `int_to_sexp` : `int` &rarr; [`sexp`](#sexp)

    Given an `int`, produces an [`Integer`](#Integer).

    ```ocaml
    # int_to_sexp 42;;
    - : sexp = Integer 42
    ```

  * <a id="sexp_to_float_opt" /> `sexp_to_float_opt` : [`sexp`](#sexp) &rarr;
    `float` `option`

    Given a [`Float`](#Float), returns a `Some` containing the internal `float`.
    Otherwise, returns `None`.

    ```ocaml
    # sexp_to_float_opt (Float 1.0);;
    - : float option = Some 1.0
    # sexp_to_float_opt (String "not a float");;
    - : float option = None
    ```

  * <a id="sexp_to_float" /> `sexp_to_float` : [`sexp`](#sexp) &rarr; `float`

    Given a [`Float`](#Float), returns the internal `float`. Otherwise, raises
    an exception.

    ```ocaml
    # sexp_to_float (Float 1.0);;
    - : float = 1.0
    # sexp_to_float (String "not a float");;
    Exception: Failure "S-Expression is not a float: \"not a float\"".
    ```

  * <a id="float_to_sexp" /> `float_to_sexp` : `float` &rarr; [`sexp`](#sexp)

    Given a `float`, produces a [`Float`](#Float).

    ```ocaml
    # float_to_sexp 4.2;;
    - : sexp = Float 4.2
    ```

  * <a id="sexp_to_string_opt" /> `sexp_to_string_opt` : [`sexp`](#sexp) &rarr;
    `string` `option`

    Given a [`String`](#String), returns a `Some` containing the internal
    `string`. Otherwise, returns `None`.

    ```ocaml
    # sexp_to_string_opt (String "hello");;
    - : string option = Some "hello"
    # sexp_to_string_opt (Integer 1);;
    - : string option = None
    ```

  * <a id="sexp_to_string" /> `sexp_to_string` : [`sexp`](#sexp) &rarr; `string`

    Given a [`String`](#String), returns the internal `string`. Otherwise,
    raises an exception.

    ```ocaml
    # sexp_to_string (String "hello");;
    - : string = "hello"
    # sexp_to_string (Integer 1);;
    Exception: Failure "S-Expression is not a string: 1".
    ```

  * <a id="string_to_sexp" /> `string_to_sexp` : `string` &rarr; [`sexp`](#sexp)

    Given a `string`, produces a [`String`](#String).

    ```ocaml
    # string_to_sexp "some string";;
    - : sexp = String "some string"
    ```

  * <a id="sexp_to_symbol_opt" /> `sexp_to_symbol_opt` : [`sexp`](#sexp) &rarr;
    `string` `option`

    Given a [`Symbol`](#Symbol), returns a `Some` containing the internal
    `string`. Otherwise, returns `None`.

    ```ocaml
    # sexp_to_symbol_opt (Symbol "hello");;
    - : string option = Some "hello"
    # sexp_to_symbol_opt (Integer 1);;
    - : string option = None
    ```

  * <a id="sexp_to_symbol" /> `sexp_to_symbol` : [`sexp`](#sexp) &rarr; `string`

    Given a [`Symbol`](#Symbol), returns the internal `string`. Otherwise,
    raises an exception.

    ```ocaml
    # sexp_to_symbol (Symbol "hello");;
    - : string = "hello"
    # sexp_to_symbol (Integer 1);;
    Exception: Failure "S-Expression is not a symbol: 1".
    ```

  * <a id="symbol_to_sexp" /> `symbol_to_sexp` : `string` &rarr; [`sexp`](#sexp)

    Given a `string`, produces a [`Symbol`](#Symbol).

    ```ocaml
    # symbol_to_sexp "foo";;
    - : sexp = Symbol "foo"
    ```


### Parsing S-Expression Patterns

  * <a id="sexp_pattern_of_string" /> `sexp_pattern_of_string` : `string` &rarr;
    [`sexp_pattern`](#sexp_pattern) `option`

    Attempts to convert a string to an [`sexp_pattern`](#sexp_pattern).

    ```ocaml
    # sexp_pattern_of_string "(lambda (SYMBOL ...) ANY ...)";;
    - : sexp_pattern option =
    Some
     (SPat [PSymbol "lambda"; SPat [SYMBOL; PSymbol "..."]; ANY; PSymbol "..."])
    # sexp_pattern_of_string "(lambda (FOO)";;
    - : sexp_pattern option = None
    ```

  * <a id="sexp_pattern_of_string_exn" /> `sexp_pattern_of_string_exn` :
    `string` &rarr; [`sexp_pattern`](#sexp_pattern)

    Attempts to convert a string to an [`sexp_pattern`](#sexp_pattern), or else
    raises an exception.

    ```ocaml
    # sexp_pattern_of_string_exn "(lambda (SYMBOL ...) ANY ...)";;
    - : sexp_pattern =
    SPat [PSymbol "lambda"; SPat [SYMBOL; PSymbol "..."]; ANY; PSymbol "..."]
    # sexp_pattern_of_string "(lambda (FOO)";;
    Exception: Failure "unterminated S-expression".
    ```

  * <a id="sexp_pattern_of_sexp" /> `sexp_pattern_of_sexp` : [`sexp`](#sexp)
    &rarr; [`sexp_pattern`](#sexp_pattern) `option`

    Attempts to convert an [`sexp`](#sexp) to an
    [`sexp_pattern`](#sexp_pattern). Ellipses ([`PSymbol`](#PSymbol) `"..."`)
    are checked to ensure they:

      * Only appear in [`SPat`](#SPat) lists.
      * Only follow non-ellipsis items.
      * Do not *start* an [`SPat`](#SPat) list.

    ```ocaml
    # sexp_pattern_of_sexp (Symbol "SYMBOL");;
    - : sexp_pattern option = Some SYMBOL
    # sexp_pattern_of_sexp (SExp [Symbol "SYMBOL"; Integer 42]);;
    - : sexp_pattern option = Some (SPat [SYMBOL; PInteger 42])
    # sexp_pattern_of_sexp (SExp [Symbol "foo"; Symbol "..."]);;
    - : sexp_pattern option = Some (SPat [PSymbol "foo"; PSymbol "..."])
    # sexp_pattern_of_sexp (Symbol "...");;
    - : sexp_pattern option = None
    # sexp_pattern_of_sexp (SExp [Symbol "foo"; Symbol "..."; Symbol "..."]);;
    - : sexp_pattern option = None
    # sexp_pattern_of_sexp (SExp [Symbol "..."; Symbol "foo"]);;
    - : sexp_pattern option = None
    ```


### Converting Whole S-Expression Patterns to Strings

  * <a id="render_string_of_sexp_pattern" /> `render_string_of_sexp_pattern` :
    [`sexp_pattern`](#sexp_pattern) &rarr; `string`

    Converts an [`sexp_pattern`](#sexp_pattern) to a string representing that
    S-Expression pattern.

    ```ocaml
    # render_string_of_sexp_pattern (SPat [PSymbol "lambda"; SPat [SYMBOL; PSymbol "..."]; ANY; PSymbol "..."]);;
    - : string = "{lambda {SYMBOL ...} ANY ...}"
    ```

  * <a id="repr_string_of_sexp_pattern" /> `repr_string_of_sexp_pattern` :
    [`sexp_pattern`](#sexp_pattern) &rarr; `string`

    Converts an [`sexp_pattern`](#sexp_pattern) to a string that is explicitly
    annotated, useful for debugging but not much else.

    ```ocaml
    # repr_string_of_sexp_pattern (SPat [PSymbol "foo"; PInteger 42; ANY]);;
    - : string = "SPat [PSymbol \"foo\"; PInteger 42; ANY]"
    ```


### Matching S-Expressions Using S-Expression Patterns

  * <a id="sexp_match" /> `sexp_match` : [`sexp_pattern`](#sexp_pattern) &rarr;
    [`sexp`](#sexp) &rarr; `bool`

    Determines whether an [`sexp`](#sexp) matches against an
    [`sexp_pattern`](#sexp_pattern).

    ```ocaml
    # sexp_match (PSymbol "foo") (Symbol "foo");;
    - : bool = true
    # sexp_match (PSymbol "bar") (Symbol "foo");;
    - : bool = false
    # sexp_match (SYMBOL) (Symbol "foo");;
    - : bool = true
    # sexp_match (STRING) (Symbol "foo");;
    - : bool = false
    # sexp_match (SPat [SYMBOL; INTEGER]) (SExp [Symbol "foo"; Integer 42]);;
    - : bool = true
    # sexp_match (SPat [SYMBOL; INTEGER]) (Symbol "foo");;
    - : bool = false
    # sexp_match (SPat [SYMBOL; PSymbol "..."]) (SExp []);;
    - : bool = true
    # sexp_match (SPat [SYMBOL; PSymbol "..."]) (SExp [Symbol "foo"]);;
    - : bool = true
    # sexp_match (SPat [SYMBOL; PSymbol "..."]) (SExp [Symbol "foo"; Symbol "bar"]);;
    - : bool = true
    # sexp_match (SPat [SYMBOL; PSymbol "..."]) (SExp [Symbol "foo"; Integer 42]);;
    - : bool = false
    ```
